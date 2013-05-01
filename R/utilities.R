
## ------------------------------------------------------------------------
## utility functions exposed to the users
## ------------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    is(x, "db.data.frame")
}

## ------------------------------------------------------------------------

## Grab a preview of the data
preview <- function(x, nrows = 100, interactive = TRUE)
{
    if (! is(x, "db.data.frame"))
        stop(deparse(substitute(x)), " must be a db.data.frame object!")

    if (is(x, "db.view") && interactive) {
        cat(deparse(substitute(x)),
            "points to a view in the database",
            dbname(conn.id(x)),
            "and it takes time to evaluate and extract a preview of it !\n")
        go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                          c("yes", "y", "no", "n"))
        if (go == "no" || go == "n") return
    }

    .db.getQuery(paste("select * from", content(x), "limit", nrows),
                 conn.id(x))
}

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
## Sampling function
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

setGeneric ("sample")

setMethod(
    "sample",
    signature(x = "db.table"),
    function (x, size, replace = FALSE, prob = NULL) {
        if (!identical(x@.key, character(0))) # has a valid key
        {

        }
        cat("To be implemented !\n")
        return (0)
    })

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.view"),
    function (x, size, replace = FALSE, prob = NULL) {
        cat("To be implemented !\n")
        return (0)
    })

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.Rquery"),
    function (x, size, replace = FALSE, prob = NULL) {
        cat("To be implemented !\n")
        return (0)
    })
