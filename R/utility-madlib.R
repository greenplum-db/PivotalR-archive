
## ------------------------------------------------------------------------
## Utility functions used by madlib
## ------------------------------------------------------------------------

## check whether newer MADlib version is used
.check.madlib.version <- function (data)
{
    ## Only newer versions of MADlib are supported
    idx <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id(data), 2]
    if (identical(.localVars$db[[idx]]$madlib.v, numeric(0)) ||
        .madlib.version.number(conn.id(data)) < 0.6)
        stop("MADlib error: Please use Madlib version 0.6 or 0.7!")
}

## ------------------------------------------------------------------------

## Analyze the formula and get each terms
.get.params <- function (formula, data)
{
    params <- .analyze.formula(formula, data)

    ## create temp table for db.Rquery objects
    is.tbl.source.temp <- FALSE
    tbl.source <- character(0)
    if (is(params$data, "db.Rquery")) {
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
                               suffix = data@.factor.suffix)

    list(data = data, params = params,
         is.tbl.source.temp = is.tbl.source.temp,
         tbl.source = tbl.source)
}

## ------------------------------------------------------------------------

## get the result
.get.res <- function (sql, tbl.output = NULL, conn.id)
{
    ## execute the linear regression
    res <- try(.db.getQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class))
        stop("Could not run SQL query !")

    ## retreive result
    if (!is.null(tbl.output)) {
        res <- try(.db.getQuery(paste("select * from", tbl.output),
                                conn.id),
                   silent = TRUE)
        if (is(res, .err.class))
            stop("Could not retreive result from SQL query !")
    }

    res
}

## ------------------------------------------------------------------------

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

groups.lm.madlib <- function (x)
{
    .get.groups(x)
}

groups.logregr.madlib <- function (x)
{
    .get.groups(x)
}

groups.lm.madlib.grps <- function (x)
{
    .get.groups.grps(x)
}

groups.logregr.madlib.grps <- function (x)
{
    .get.groups.grps(x)
}
