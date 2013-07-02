
## ------------------------------------------------------------------------
## Utility functions used by madlib
## ------------------------------------------------------------------------

## check whether newer MADlib version is used
.check.madlib.version <- function (data)
{
    ## Only newer versions of MADlib are supported
    idx <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id(data), 2]
    if (identical(.localVars$db[[idx]]$madlib.v, numeric(0)) ||
        .madlib.version.number(conn.id(data)) < 0.6 ||
        .madlib.version.number(conn.id(data)) > 0.7)
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
.get.res <- function (sql, tbl.output, conn.id)
{
    ## execute the linear regression
    res <- try(.db.getQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class))
        stop("Could not run MADlib linear regression !")

    ## retreive result
    res <- try(.db.getQuery(paste("select * from", tbl.output), conn.id),
               silent = TRUE)
    if (is(res, .err.class))
        stop("Could not retreive MADlib linear regression result !")

    res
}
