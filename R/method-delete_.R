
## ------------------------------------------------------------------------
## delete db.data.frame objects
## ------------------------------------------------------------------------

setGeneric (
    "delete",
    def = function (x, ...) standardGeneric("delete"),
    signature = "x")

## ------------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "db.data.frame"),
    def = function (x) {
        .db.removeTable(content(x), conn.id(x))
        rm(x)
    })

## ------------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "db.Rquery"),
    def = function (x) {
        rm(x)
    })
