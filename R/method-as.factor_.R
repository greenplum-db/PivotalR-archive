
## ------------------------------------------------------------------------
## as.factor
## ------------------------------------------------------------------------

setGenric ("as.factor")

setMethod (
    "as.factor",
    signature(x = "db.obj"),
    function (x) {
        if (length(x@.col.name))
    },
    valueClass = "db.Rquery")
