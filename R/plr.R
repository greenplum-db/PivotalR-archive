## PL/R support

## ------------------------------------------------------------

## Helper functions

## extract the prefix expression
.ras <- function(x) {
    res <- as.list(x)
    if (identical(res[[1]], x)) return (x)
    for (i in 1:length(res))
        if (is.language(res[[i]]))
            res[[i]] <- ras(res[[i]])
    res
}

## -----------------------------------------------------

## Extract types and the function body
.plr.parser <- function(fun) {
    str <- as.character(enquote(
        as.list(substitute(fun))[[3]]))[[2]]
    ## str <- as.character(enquote(fun))[2]
    w <- ras(fun)
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
                if (names(declare)[i] == "")
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
         types = declare)
}

## ------------------------------------------------------------

## dummy function to make decalre expression valid
declare <- function(...) return (invisible(..1))

## ------------------------------------------------------------

## Map function
plr.chunkmap <- function(data, INDICES, FUN, ...)
{
    parse <- .plr.parser(FUN)
    fun.body <- parse$fun.str
    fun.types <- parse$types

    ## function arguments and return types
    arg.types <- fun.types[Filter(function(s) s != "", names(fun.types))]
    ret.type <- fun.types[which(names(fun.types) == "")[1]][[1]]

    ## grouped intermediate table
    if (is.null(attr(data, "grouped.data")))
        grp.data <- as.db.data.frame(by(data[,names(arg.types)], INDICES, colAgg))
    else
        ## Already done that, no need to do it any more
        grp.data <- attr(data, "grouped.data")

    ## Create return compisite type
    db.rettype <- .create_plr_rettype(ret.type) # name of return type in database

    ## Create a temporary PL/R function
    plr.func <- .plr(fun.body, arg.types, db.rettype)

    ## Execute the PL/R function and create the result table
    models <- as.db.data.frame(plr.func(grp.data), .unique.string())
    attr(models, "group.data") <- grp.data
    models
}

## ------------------------------------------------------------

## Reduce function
plr.reduce <- function(data, FUN, ...)
{
    agg <- plr.agg(FUN)
    agg(data)
}

## ------------------------------------------------------------

## Create a PL/R function
plr <- function(FUN)
{
    parse <- .plr.parser(FUN)
    fun.body <- parse$fun.str
    fun.types <- parse$types

    ## function arguments and return types
    arg.types <- fun.types[Filter(function(s) s != "", names(fun.types))]
    ret.type <- fun.types[which(names(fun.types) == "")[1]][[1]]

    db.rettype <- .create_plr_rettype(ret.type) # name of return type in database

    ## Create a temporary PL/R function
    .plr(fun.body, arg.types, db.rettype)
}

## ------------------------------------------------------------

.create_plr_rettype <- function(ret.type)
{

}

## ------------------------------------------------------------

## Create a PL/R aggregate
plr.agg <- function(FUN)
{

}

## ------------------------------------------------------------

.plr <- function()
{

}
