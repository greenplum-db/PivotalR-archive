
## -----------------------------------------------------------------------
## How to display the db objects
## -----------------------------------------------------------------------

setGeneric ("print", signature = "x")

setMethod (
    "print",
    signature (x = "db.data.frame"),
    function (x) {
        if (x@.table.type == "LOCAL TEMPORARY") {
            if (is(x, "db.view")) {
                temp <- "Temp view"
                ms <- "     "
            } else {
                temp <- "Temp table"
                ms <- "      "
            }
        } else {
            if (is(x, "db.view")) {
                temp <- "View"
                ms <- ""
            } else {
                temp <- "Table"
                ms <- " "
            }
        }
        cat(temp, "       :    ", x@.content, "\n", sep = "")
        cat("Database", ms, "   :    ", dbname(x@.conn.id), "\n", sep = "")
        cat("Host", ms, "       :    ", host(x@.conn.id), "\n", sep = "")
        cat("Connection", ms, " :    ", x@.conn.id, "\n", sep = "")
    })

## -----------------------------------------------------------------------

## setGeneric ("show", signature = "object")

setMethod (
    "show",
    signature (object = "db.data.frame"),
    function (object) {
        print(object)
    })

## -----------------------------------------------------------------------

## print method for db.Rquery objects

setMethod (
    "print",
    signature (x = "db.Rquery"),
    function (x) {
        if (identical(content(x), character(0))) {
            cat("NULL\n")
            return (NULL)
        }
        cat("A temporary object in R derived from ", x@.source, "\n", sep = "")
        cat("Database   :    ", dbname(x@.conn.id), "\n", sep = "")
        cat("Host       :    ", host(x@.conn.id), "\n", sep = "")
        cat("Connection :    ", x@.conn.id, "\n", sep = "")
        cat("--\n")
        cat("If you want to make it point to a real object in database,\n")
        cat("please use the function as.db.data.frame.\n")
        cat("See help(as.db.data.frame) for more.\n")
    })

## -----------------------------------------------------------------------

setMethod (
    "show",
    signature (object = "db.Rquery"),
    function (object) {
        print(object)
    })
