## Helper functions needed by the array operations
## Only for db.obj
## Every platform should have its own helper functions

## ----------------------------------------------------------------------

setGeneric(".get.where",
           signature = "x"
           def = function(x) standardGeneric(".get.where"))

## ----------------------------------------------------------------------

## extract where part
setMethod(
    ".get.where",
    signature(x = "db.obj"),
    function(x) {
        if (is(x, "db.Rquery"))
            x@.where
        else
            ""
    },
    valueClass = "character")

## ----------------------------------------------------------------------

setGeneric(".get.sort", signature = "x",
           def = function(x) standardGeneric(".get.sort"))

## ----------------------------------------------------------------------

setMethod(
    ".get.sort",
    signature(x = "db.obj"),
    function(x) {
        if (is(x, "db.data.frame")) {
            sort <- list(by = "", order = "", str = "")
        } else {
            sort <- x@.sort
        }
        sort
    },
    valueClass = "character")
