
## ------------------------------------------------------------------------
## Summary of a db.obj
## ------------------------------------------------------------------------

madlib.summary <- function (x, target.cols = NULL, grouping.cols = NULL,
                            get.distinct = TRUE, get.quartiles = TRUE,
                            ntile = NULL, n.mfv = 10, estimate = TRUE,
                            interactive = TRUE)
{
    if (!is(x, "db.obj"))
        stop("Cannot operate on non db.obj objects!")

    if ((!is.null(target.cols) && !all(target.cols %in% names(x))) ||
        (!is.null(grouping.cols) && !all(grouping.cols %in% names(x))))
        stop("target.cols or grouping.cols has columns that are not in ",
             "the data!")
    
    if (is(x, "db.view") || is(x, "db.Rquery")) {
        if (interactive) {
            cat(deparse(substitute(x)),
                "does not point to a table in the database",
                dbname(conn.id(x)),
                "and it might take time to create a table and get the summary for this!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }
        tbl <- .unique.string()
        if (is(x, "db.Rquery"))
            tmp <- as.db.data.frame(x, tbl, conn.id(x), FALSE
                                    is.temp = TRUE,
                                    verbose = interactive,
                                    pivot.factor = FALSE)
        else
            .db.getQuery(paste("create temp table", tbl,
                               "as select * from", content(x)), conn.id(x))
        
    } else
        tbl <- content(x)

    
}
