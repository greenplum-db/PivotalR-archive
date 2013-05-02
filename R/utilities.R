
## ------------------------------------------------------------------------
## utility functions exposed to the users
## ------------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    is(x, "db.data.frame")
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
