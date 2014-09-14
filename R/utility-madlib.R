## -----------------------------------------------------------------------
## Utility functions used by madlib
## -----------------------------------------------------------------------

## check whether newer MADlib version is used
.check.madlib.version <- function (data, allowed.version = 0.6)
{
    ## Only newer versions of MADlib are supported
    conn.id <- conn.id(data)
    idx <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    db <- .get.dbms.str(conn.id)
    if (db$db.str != "HAWQ" || !grepl("^1\\.1", db$version.str)) {
        if (identical(.localVars$db[[idx]]$madlib.v, numeric(0)) ||
            .madlib.version.number(conn.id) < allowed.version)
            stop("MADlib error: Please use Madlib version v",
                 allowed.version, " or newer !")
    }
}

## -----------------------------------------------------------------------

## Analyze the formula and get each terms
.get.params <- function (formula, data, na.action = NULL, na.as.level = FALSE,
                         create.dummy = TRUE)
{
    n <- ncol(data)
    params <- .analyze.formula(formula, data)

    if (!is.null(na.action)) {
        params$data <- na.action(params$data, vars =
                                 with(params, c(origin.dep, grp.vars,
                                                origin.ind,
                                                names(data)[data@.is.factor])))
    }

    ## create temp table for db.Rquery objects
    is.tbl.source.temp <- FALSE
    tbl.source <- character(0)
    no.factor <- FALSE
    if (is(params$data, "db.Rquery")) {
        if (create.dummy) {
            tbl.source <- .unique.string()
            is.tbl.source.temp <- TRUE
            data <- as.db.data.frame(x = params$data,
                                     table.name = tbl.source,
                                     is.temp = FALSE, verbose = FALSE,
                                     distributed.by = params$data@.dist.by,
                                     factor.full = params$factor.full,
                                     na.as.level = na.as.level)
        } else {
            if (! is(data, "db.table")) {
                tbl.source <- .unique.string()
                data <- as.db.data.frame(x = params$data,
                                         table.name = tbl.source,
                                         is.temp = FALSE, verbose = FALSE,
                                         distributed.by = params$data@.dist.by,
                                         factor.full = params$factor.full,
                                         na.as.level = na.as.level, pivot = FALSE)
                no.factor <- TRUE
            }
        }
    } else if (is(params$data, "db.view")) {
        tbl.source <- .unique.string()
        is.tbl.source.temp <- TRUE
        data <- as.db.data.frame(x = params$data,
                                 table.name = tbl.source,
                                 is.temp = FALSE, verbose = FALSE)
    }

    is.factor <- data@.is.factor
    cols <- names(data)

    params <- .analyze.formula(formula, data, params$data, refresh = TRUE,
                               is.factor = is.factor, cols = cols,
                               suffix = data@.factor.suffix,
                               grp.vars = params$grp.vars,
                               grp.expr = params$grp.expr, no.factor = no.factor)

    list(data = data, params = params,
         is.tbl.source.temp = is.tbl.source.temp,
         tbl.source = tbl.source, is.factor = is.factor[seq(n)],
         factor.ref = data@.factor.ref)
}

## -----------------------------------------------------------------------

## get the result
.get.res <- function (sql, tbl.output = NULL, conn.id, warns = NULL)
{
    ## execute the linear regression
    res <- try(.db.getQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class)) {
        if (!is.null(warns)) .restore.warnings(warns)
        stop("Could not run SQL query !")
    }

    ## retreive result
    if (!is.null(tbl.output)) {
        res <- try(.db.getQuery(paste("select * from", tbl.output),
                                conn.id),
                   silent = TRUE)
        if (is(res, .err.class)) {
            if (!is.null(warns)) .restore.warnings(warns)
            stop("Could not retreive result from SQL query !")
        }
    }

    res
}

## -----------------------------------------------------------------------

.get.groups <- function (x)
{
    if (length(x$grp.cols) != 0) {
        res <- list()
        for (col in x$grp.cols)
            res[[col]] <- x[[col]]
        return (res)
    } else {
        return (NULL)
    }
}

## -----------------------------------------------------------------------

.get.groups.grps <- function (x)
{
    n <- length(x)
    for (i in seq_len(n)) {
        if (i == 1)
            res <- .get.groups(x[[1]])
        else {
            tmp <- .get.groups(x[[i]])
            for (col in x[[i]]$grp.cols)
                res[[col]] <- c(res[[col]], tmp[[col]])
        }
    }
    res
}

## ----------------------------------------------------------------------

groups <- function (x) UseMethod("groups", x)

## -----------------------------------------------------------------------

groups.lm.madlib <- function (x)
{
    .get.groups(x)
}

## -----------------------------------------------------------------------

groups.logregr.madlib <- function (x)
{
    .get.groups(x)
}

## -----------------------------------------------------------------------

groups.lm.madlib.grps <- function (x)
{
    .get.groups.grps(x)
}

## -----------------------------------------------------------------------

groups.logregr.madlib.grps <- function (x)
{
    .get.groups.grps(x)
}

## ----------------------------------------------------------------------

## delete all __madlib_temp_* tables from a database
clean.madlib.temp <- function(conn.id = 1)
{
    for (tbl in db.objects(
        .unique.pattern(),
        conn.id=conn.id))
        delete(tbl, conn.id=conn.id, cascade = TRUE)
}

## ----------------------------------------------------------------------

## Compute the first-derivative of any functino analytically
## And return the result as a string
## Will be used in computing margins
.parse.deriv <- function (expr.str, var)
{
    formula <- formula(paste("~", expr.str))
    x <- deriv(formula, var)
    lst0 <- deparse(x)
    lst0 <- lst0[2:(length(lst0)-1)]
    lst <- character(0)
    ignore <- FALSE
    for (i in 1:length(lst0)) {
        if (grepl("\\.value <- ", lst0[i]) ||
            grepl("\\.grad <- ", lst0[i]) ||
            grepl("attr\\(\\.value,", lst0[i])) {
            ignore <- TRUE
            next
        }
        if (ignore) {
            if (!grepl("<-", lst0[i]))
                next
            else
                ignore <- FALSE
        }
        if (grepl("<-", lst0[i])) {
            lst <- c(lst, lst0[i])
        } else {
            if (i != 1)
                lst[length(lst)] <- paste(lst[length(lst)], lst0[i])
        }
    }

    env <- lapply(lst, function(x) eval(parse(
        text = paste("quote(", strsplit(x, "\\s*<-\\s*")[[1]][2],
        ")", sep = ""))))

    names(env) <- sapply(lst, function(x)
                         gsub("^\\s*", "",
                              strsplit(x, "\\s*<-\\s*")[[1]][1]))

    k <- which(names(env) == paste(".grad[, \"", gsub("\"", "\\\\\\\"", var),
                    "\"]", sep = ""))
    pre.res <- ""
    res <- env[[k]]
    while (!identical(pre.res, res)) {
        for (i in 1:length(env))
            env[[i]] <- eval(parse(text = paste("substitute(",
                                   paste(deparse(env[[i]]),
                                         collapse = " "),
                                   ", env)", sep = "")))
        pre.res <- res
        res <- env[[k]]
    }
    gsub("\\s+", " ", paste(deparse(res), collapse = " "))
}
