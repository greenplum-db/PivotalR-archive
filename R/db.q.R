## ----------------------------------------------------------------------
## directly execute SQL
## ----------------------------------------------------------------------

.db <- db <- db.q <- function(..., nrows = 100, conn.id = 1,
                              sep = " ", verbose = TRUE)
{
    if (!.is.conn.id.valid(conn.id))
        stop(conn.id, " is not a valid connection ID. ",
             "Use db.list() to view the available choices")

    sql <- paste(..., sep = sep)
    if (verbose) {
        message("Executing in database connection ", conn.id, ":\n")
        cat(sql, "\n\n")
    }

    warns <- .suppress.warnings (conn.id, "warning")

    if (is.null(nrows) || (is.character(nrows) && nrows == "all")
        || nrows <= 0) nrows <- -1

    res <- tryCatch(
        .db.sendQuery(sql, conn.id),
        error = function(c) c,
        interrupt = function(c) .restore.warnings(warns))

    if (is(res, "error")) {
        .restore.warnings(warns)
        if (!verbose) {
            message("Executing in database connection ", conn.id, ":\n")
            cat(sql, "\n\n")
        }
        stop(res$message)
    }

    dat <- tryCatch(
        .db.fetch(res, nrows),
        error = function(c) c,
        finally = {
            .db.clearResult(res)
            .restore.warnings(warns)
        })

    if (is(dat, "error")) {
        ## do nothing
    } else
        return (dat)
}
