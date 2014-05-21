## PL/R support

## ------------------------------------------------------------

## Helper functions

## extract the prefix expression
.ras <- function(x) {
    res <- as.list(x)
    if (identical(res[[1]], x)) return (x)
    for (i in 1:length(res))
        if (is.language(res[[i]]))
            res[[i]] <- .ras(res[[i]])
    res
}

## -----------------------------------------------------

## Extract types and the function body
.plr.parser <- function(fun) {
    s <- as.list(as.list(fun)[[2]])
    args <- names(s)
    args <- args[args != ""]
    str <- paste(as.character(s[[length(s)]]), collapse = "\n")
    w <- .ras(as.list(fun)[[2]])
    w <- w[[length(w)]]
    declare <- Filter(function(s)
                      {
                          if (length(s) == 1)
                              s == "declare"
                          else
                              s[[1]] == "declare"
                      }, w)[[1]]

    a <- sapply(2:length(declare), function (i) {
                dec <- paste("[\"']*", declare[i], "[\"']*", sep = "")
                #dec <- declare[i]
                if (names(declare)[i] == ""  || is.null(names(declare)[i]))
                    dec
                else
                    paste(names(declare)[i],
                          "[\\s\\t\\n]*=[\\t\\s\\n]*",
                          dec,
                          sep = "")
                      })
    s <- paste("declare[\\t\\s\\n]*\\([\\t\\s\\n]*",
               paste(a, collapse = ",[\\t\\s\\n]*"),
               "[\\t\\s\\n]*\\)[\\t\\s\\n]*",
               sep = "")

    declare[[1]] <- NULL
    list(fun.str = gsub("^\\{|\\}$", "",
                        gsub("\\s*declare\\s*\\(.*\\)\\n", "", str, perl = T)),
         types = declare,
         args = args)
}

## ------------------------------------------------------------

## dummy function to make decalre expression valid
declare <- function(...) return (invisible(..1))

## ------------------------------------------------------------

## Reduce function
plr.reduce <- function(data, FUN, ...)
{
    agg <- plr.agg(FUN)
    agg(data)
}

## ------------------------------------------------------------

## Create a PL/R function
plr <- function(FUN, conn.id = 1)
{
    parse <- .plr.parser(enquote(FUN))
    fun.body <- parse$fun.str
    fun.types <- parse$types
    args <- parse$args

    ## function arguments and return types
    arg.types <- fun.types[Filter(function(s) s != "", names(fun.types))]
    ret.type <- fun.types[which(names(fun.types) == "")[1]][[1]]

    db.rettype <- .create_plr_rettype(ret.type, conn.id = conn.id) # name of return type in database

    if (identical(arg.types, list()))
        args.types <- rep(gsub("setof ", "", db.rettype), length(args))
    else
        arg.types <- as.character(as.vector(arg.types))

    ## Create a temporary PL/R function
    db.func <- .unique.string()
    .db("
        create function ", db.func, "(",
        paste(paste(args, ' ', arg.types, sep = ''), collapse = ", "),
        ") returns ", db.rettype, " as $$ ", fun.body, "$$ language plr",
        conn.id = conn.id, verbose = FALSE, sep = "")

    func <- function(data) {
        if (conn.id(data) != conn.id)
            stop("This function can only be run in connection ", conn.id)

        by.names <- attr(data, "grp")

        if (is(data, "db.Rquery")) {
            data <- as.db.data.frame(data, verbose = FALSE)
            is.temp <- TRUE
        } else
            is.temp  <- FALSE

        by.str <- if (is.null(by.names)) "" else paste(paste(names(data)[1:length(by.names)], collapse = ", "), ", ", sep = "")
        args <- sapply(args, function(s) if (! (s %in% names(data))) paste(s, "_array_agg", sep = '') else s)
        func.str <- paste(db.func, "(", paste(args, collapse = ", ", sep = ''),
                          ") as result", sep = "")
        func.str <- paste(by.str, func.str, sep = "")

        sql <- paste("select ", func.str, " from ", content(data), sep = "")

        if (grepl(.unique.pattern(), db.rettype, perl = T))
            sql <- paste("select ", by.str, "(result).* from (", sql, ") s", sep = "")

        result.table <- .unique.string()
        .db("create table ", result.table, " as ", sql,
            conn.id = conn.id, verbose = FALSE, sep = "")

        if (is.temp) delete(data)

        db.data.frame(result.table, conn.id = conn.id, verbose = FALSE)
    }

    attr(func, "plr") <- db.func # can be used for deletion of the function
    attr(func, "plr.ret") <- gsub("setof", "", db.rettype)
    attr(func, "plr.args") <- args
    func
}

## ------------------------------------------------------------

## Create the return type
.create_plr_rettype <- function(ret.type, conn.id)
{
    if (is(ret.type, "list")) {
        args <- names(ret.type)[-1] # argument names
        types <- sapply(2:length(ret.type), function(i) as.character(ret.type[[i]]))
        type.name <- .unique.string()
        .db("create type ", type.name, " as (",
            paste(paste(args, types), collapse = ", "), ")",
            conn.id = conn.id, verbose = FALSE, sep = "")
        return (paste("setof", type.name))
    } else {
        return (as.character(ret.type))
    }
}

## ------------------------------------------------------------

## Create a PL/R aggregate
plr.agg <- function(FUN, conn.id = 1)
{
    plr.fun <- plr(FUN, conn.id = conn.id) # R function
    plr.db.fun <- attr(plr.fun, "plr") # PL/R function in database
    plr.rettype <- attr(plr.fun, "plr.ret") # return type
    plr.args <- attr(plr.fun, "plr.args")

    agg_name = .unique.string()
    .db("
        create aggregate ", agg_name, " (
            SType = ", plr.rettype, ",
            SFunc = ", plr.db.fun, "
        )",
        conn.id = conn.id, verbose = FALSE, sep = "")

    func <- function(data) {
        if (conn.id(data) != conn.id)
            stop("This function can only be run in connection ", conn.id)

        if (is(data, "db.Rquery")) {
            data <- as.db.data.frame(data)
            is.temp <- TRUE
        } else
            is.temp  <- FALSE

        func.str <- paste(db.func, "(", paste(plr.args, collapse = ", ", sep = ''),
                          ") as result", sep = "")

        sql <- paste("select ", func.str, " from ", content(data), sep = "")

        if (grepl(.unique.pattern(), db.rettype, perl = T))
            sql <- paste("select (result).* from (", sql, ") s", sep = "")

        result.table <- .unique.string()
        .db("create table ", result.table, " as ", sql,
            conn.id = conn.id, verbose = FALSE, sep = "")

        if (is.temp) delete(data)

        db.data.frame(result.table, conn.id = conn.id, verbose = FALSE)
    }

    attr(func, "plr") <- agg_name
    func
}


