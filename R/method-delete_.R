
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
    def = function (x, cascade = FALSE) {
        ## .db.removeTable(content(x), conn.id(x))
        s <- delete(content(x), conn.id(x), x@.table.type == "LOCAL TEMPORARY",
                    cascade)
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
    def = function (x, conn.id = 1, is.temp = FALSE, cascade = FALSE) {
        origin.x <- x
        warn.r <- getOption("warn")
        options(warn = -1)

        exists <- db.existsObject(x, conn.id, is.temp)
        if (length(exists) == 2)
            if (! exists[[1]]) {
                options(warn = warn.r) # reset R warning level
                return (FALSE)
            } else
                x <- exists[[2]]
        else
            if (! exists) {
                options(warn = warn.r) # reset R warning level
                return (FALSE)
            } else {
                x <- strsplit(x, "\\.")[[1]]
                if (length(x) != 2) {
                    schemas <- arraydb.to.arrayr(
                        .db.getQuery("select current_schemas(True)", conn.id),
                        type = "character")
                    table_schema <- character(0)
                    for (schema in schemas)
                        if (.db.existsTable(c(schema, x), conn.id))
                            table_schema <- c(table_schema, schema)
                    if (identical(table_schema, character(0))) {
                        options(warn = warn.r) # reset R warning level
                        return (FALSE)
                    }
                    schema.str <- strsplit(table_schema, "_")
                    for (i in seq_len(length(schema.str))) {
                        str <- schema.str[[i]]
                        if (str[1] != "pg" || str[2] != "temp") {
                            x <- c(table_schema[i], x)
                            break
                        }
                    }
                }
                if (length(x) == 1) {
                    options(warn = warn.r) # reset R warning level
                    return (FALSE)
                }
            }
        ## .db.removeTable(x, conn.id)
        table <- paste(x[1], ".", x[2], sep = "")
        if (cascade) cascade.str <- " cascade"
        else cascade.str <- ""

        if (.is.view(x, conn.id))
            type.str <- "view "
        else
            type.str <- "table "
        sql <- paste("drop ", type.str, table, cascade.str, sep = "")
        res <- tryCatch(.db.getQuery(sql, conn.id),
                        error = function(e) { success <<- FALSE })
        exists <- db.existsObject(origin.x, conn.id, is.temp)
        
        options(warn = warn.r) # reset R warning level
        if (length(exists) == 2)
            if (! exists[[1]]) 
                return (TRUE)
            else
                return (FALSE)
        else
            if (! exists)
                return (TRUE)
            else
                return (FALSE)
    })
