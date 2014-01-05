## ----------------------------------------------------------------------
## directly execute SQL
## ----------------------------------------------------------------------

db.q <- function(..., nrows = 100, conn.id = 1, sep = " ",
                 verbose = TRUE)
{
    warns <- .suppress.warnings (conn.id, "warning")
    sql <- paste(..., sep = sep)
    if (verbose) {
        message("Executing in database connection ", conn.id, ":")
        cat(sql, "\n\n")
    }
    ## if (!is.character(sql))
    ##     stop("can only execte a SQL query string!")

    if (is.null(nrows) || (is.character(nrows) && nrows == "all")
        || nrows <= 0) nrows <- -1
    res <- try(.db.sendQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class)) {
        .restore.warnings(warns)
        stop(res[1])
    }
    dat <- try(.db.fetch(res, nrows), silent = TRUE)
    if (is(dat, .err.class)) {
        .db.clearResult(res)
        .restore.warnings(warns)
        ## stop(dat[1])
    } else {
        .db.clearResult(res)
        .restore.warnings(warns)
        dat
    }
}
