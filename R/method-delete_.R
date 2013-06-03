
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
        s <- delete(content(x), conn.id(x), x@.table.type == "LOCAL TEMPORARY")
        if (s) {
            rm(x)
            return (TRUE)
        } else
            return (FALSE)
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
                return (FALSE)
            else
                x <- exists[[2]]
        else
            if (! exists)
                return
            else {
                x <- strsplit(x, "\\.")[[1]]
                if (length(x) != 2) {
                    schemas <- arraydb.to.arrayr(
                        .db.getQuery("select current_schemas(True)", conn.id),
                        type = "character")
                    table_schema <- character(0)
                    for (schema in schemas)
                        if (.db.existsTable(c(schema, x), conn.id))
                            table_schema <- c(table_schema, schema)

                    if (identical(table_schema, character(0))) return (FALSE)
                    schema.str <- strsplit(table_schema, "_")
                    for (i in seq_len(length(schema.str))) {
                        str <- schema.str[[i]]
                        if (str[1] != "pg" || str[2] != "temp") {
                            x <- c(table_schema[i], x)
                            break
                        }
                    }
                }
                if (length(x) == 1) return (FALSE)
            }
        .db.removeTable(x, conn.id)
    })
