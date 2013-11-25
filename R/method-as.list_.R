## ----------------------------------------------------------------------
## Expand the db.obj object
## It is the opposite of cbind
## ----------------------------------------------------------------------

setGeneric("as.list")

setMethod(
    "as.list",
    signature(x = "db.obj"),
    function (x, array = FALSE, ...)
{
    res <- NULL
    for (i in seq_len(length(names(x)))) {
        if (x@.col.data_type[i] == "array" && array) {
            len <- array.len(x[,i])
            for (j in seq_len(len)) res <- c(res, x[,i][j])
        } else {
            res <- c(res, x[,i])
        }
    }
    res
})

## ----------------------------------------------------------------------

## measure the length of array
array.len <- function (x)
{
    if (!is(x, "db.obj") || length(names(x)) != 1 ||
        x@.col.data_type != "array")
        stop(deparse(substitute(x)), " must be a db.obj object containing a ",
             "single array column!")
    x <- x[,]
    ## if (x@.source == x@.parent)
    ##     parent <- x@.parent
    ## else
    ##     parent <- paste("(", x@.parent, ") s", sep = "")
    ## where <- x@.where
    ## if (x@.where != "")
    ##     where.str <- paste(" where", x@.where)
    ## else
    ##     where.str <- ""
    ## arr <- .get.array.elements(x@.expr, parent, where.str, conn.id(x))
    ## length(arr)
    length(names(x))
}
