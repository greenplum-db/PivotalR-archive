## ----------------------------------------------------------------------
## directly execute SQL
## ----------------------------------------------------------------------

db.q <- function(..., nrows = 100, conn.id = 1, sep = " ",
                 verbose = TRUE)
{
    sql <- paste(..., sep = sep)
    if (verbose) {
        message("Executing in database connection ", conn.id, ":")
        cat(sql, "\n\n")
    }
    ## if (!is.character(sql))
    ##     stop("can only execte a SQL query string!")

    res <- .db.sendQuery(sql, conn.id)
    if (is.null(nrows) || (is.character(nrows) && nrows == "all")
        || nrows <= 0) nrows <- -1
    dat <- try(.db.fetch(res, nrows), silent = TRUE)
    if (is(dat, .err.class))
        .db.clearResult(res)
    else {
        .db.clearResult(res)
        dat
    }
}
