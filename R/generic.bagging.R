
## ------------------------------------------------------------------------
## Bagging method, not a wrapper of MADlib function
## ------------------------------------------------------------------------

generic.bagging <- function (train, data, nbags = 10, fraction = 1)
{
    if (fraction > 1)
        stop("fraction cannot be larger than 1!")
    if (!is(data, "db.obj"))
        stop("data must be a db.obj!")
    
    n <- dim(data)[1]
    size <- as.integer(n * fraction)

    res <- list()
    for (i in 1:nbags) {
        data.use <- sample(data, size, replace = TRUE)
        res[[i]] <- train(data = data.use)
    }
    class(res) <- "bagging.model"
    res
}

## ------------------------------------------------------------------------

predict.bagging.model <- function (object, newdata, combine = "mean",
                                   ...)
{
    l <- length(object)
    pred <- list()
    for (i in seq_len(l))
        pred[[i]] <- predict(object[[i]], newdata)

    if (combine == "mean") {
        for (i in seq_len(l)) {
            if (i == 1)
                res <- pred[[i]]
            else
                res <- res + pred[[i]]
        }
        res / l
    } else if (combine == "vote") {
        res.type <- pred[[1]]@.col.data_type
        res.udt.name <- pred[[1]]@.col.udt_name
        if (res.type %in% .int.types)
            func.suffix <- "integer"
        else if (res.type %in% .num.types)
            func.suffix <- "double precision"
        else if (res.type %in% .txt.types)
            func.suffix <- "text"
        else if (res.type == "boolean")
            func.suffix <- "boolean"
        else
            stop("the result type ", res.type,
                 " is not supported for vote!")
        
        func <- .load.func(paste("find_majority_",
                           strsplit(func.suffix, " ")[[1]][1], sep = ""),
                           conn.id(newdata))
        
        arr.str <- "array["
        for (i in seq_len(l)) {
            arr.str <- paste(arr.str, "(", pred[[i]]@.expr, ")::",
                             func.suffix, sep = "")
            if (i < l) arr.str <- paste(arr.str, ", ", sep = "")
            else arr.str <- paste(arr.str, "]", sep = "")
        }

        if (is(newdata, "db.data.frame")) {
            tbl <- content(newdata)
            src <- tbl
            parent <- src
            where <- ""
            where.str <- ""
            sort <- list(by = "", order = "", str = "")
        } else {
            if (newdata@.source == newdata@.parent)
                tbl <- newdata@.parent
            else
                tbl <- paste("(", newdata@.parent, ") s", sep = "")
            src <- newdata@.source
            parent <- newdata@.parent
            where <- newdata@.where
            if (where != "") where.str <- paste(" where", where)
            else where.str <- ""
            sort <- newdata@.sort
        }

        expr <- paste(func, "(", arr.str, ")", sep = "")
        
        sql <- paste("select ", expr, " as bagging_predict from ",
                     tbl, where.str, sort$str, sep = "")

        new("db.Rquery",
            .content = sql,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(newdata),
            .col.name = "madlib_predict",
            .key = character(0),
            .col.data_type = res.type,
            .col.udt_name = res.udt.name,
            .where = where,
            .is.factor = FALSE,
            .factor.suffix = "",
            .sort = sort)
    } else
        stop("combine method must be \"mean\" or \"vote\"!")
}

## ------------------------------------------------------------------------

## load a SQL function from inst/sql/
.load.func <- function (funcname, conn.id)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    if (!is.null(.localVars$db[[id]]$func)) {
        k <- which(.localVars$db[[id]]$func[,1] == funcname)
        if (length(k) != 0)
            return (.localVars$db[[id]]$func[k,2])
    }
    
    .localVars$pkg.path <- path.package(.this.pkg.name)
    sql.file <- paste(.localVars$pkg.path, "/sql/", "funcname",
                      ".sql_in", sep = "")
    use.name <- .unique.string()
    tmp.file <- paste("/tmp/", use.name, ".sql_in", sep = "")
    old.name <- paste("pg_temp.", funcname, sep = "")
    new.name <- paste("pg_temp.", use.name, sep = "")
    system(paste("sed -e \"s/", old.name, "/", new.name, "/g\" ", sql.file,
                 " > ", tmp.file, sep = ""))
    cmd <- paste(scan(tmp.file, what = 'a', sep = "\n"), collapse = "\n")
    res <- .db.getQuery(cmd, conn.id)
    system(paste("rm -f ", tmp.file, sep = ""))
    
    if (is.null(.localVars$db[[id]]$func))
        .localVars$db[[id]]$func <- c(funcname, use.name)
    else
        .localVars$db[[id]]$func <- rbind(.localVars$db[[id]]$func,
                                          c(funcname, use.name))

    paste("pg_temp.", use.name, sep = "")
}
