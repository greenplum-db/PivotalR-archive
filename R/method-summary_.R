
## ------------------------------------------------------------------------
## Summary of a db.obj
## ------------------------------------------------------------------------

setGeneric ("summary")

setMethod (
    "summary",
    signature(object = "db.obj"),
    function (object, ...) {
        stop("need a definition for the method here")
    },
    valueClass = "list")
