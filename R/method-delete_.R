
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
        ## .db.removeTable(content(x), conn.id(x))
        delete(content(x), conn.id(x), x@.table.type == "LOCAL TEMPORARY")
        rm(x)
    })

## ------------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "db.Rquery"),
    def = function (x) {
        rm(x)
    })

## ------------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "character"),
    def = function (x, conn.id = 1, is.temp = FALSE) {
        exists <- db.existsObject(x, conn.id, is.temp)
        if (length(exists) == 2)
            if (! exists[[1]])
                return
            else
                x <- exists[[2]]
        else
            if (! exists)
                return
            else
                x <- strsplit(x, "\\.")[[1]]
        .db.removeTable(x, conn.id)
    })
