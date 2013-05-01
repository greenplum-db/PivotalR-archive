
## ------------------------------------------------------------------------
## How to display the db objects
## ------------------------------------------------------------------------

setGeneric ("print", signature = "x")

setMethod (
    "print",
    signature (x = "db.data.frame"),
    function (x) {
        if (x@.table.type == "LOCAL TEMPORARY") {
            if (is(x, "db.view"))
                temp <- "Temp view"
            else
                temp <- "Temp table"
        } else {
            if (is(x, "db.view"))
                temp <- "View"
            else
                temp <- "Table"
        }
        cat(temp, ": ", x@.content, "\n", sep = "")
        cat("Database: ", dbname(x@.conn.id), "\n", sep = "")
        cat("Host: ", host(x@.conn.id), "\n", sep = "")
    })

## ------------------------------------------------------------------------

## setGeneric ("show", signature = "object")

setMethod (
    "show",
    signature (object = "db.data.frame"),
    function (object) {
        print(object)
    })
