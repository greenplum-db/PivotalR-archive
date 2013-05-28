
## ------------------------------------------------------------------------
## as.factor
## ------------------------------------------------------------------------

setGeneric ("as.factor")

setMethod (
    "as.factor",
    signature(x = "db.obj"),
    function (x) {
        if (length(x@.col.name) != 1)
            stop("Cannot coerce multiple columns into factor!")
        if (is(x, "db.data.frame")) {
            new("db.Rquery",
                .content = paste("select \"", x@.col.name, "\" from \"",
                content(x), "\""),
                .expr = paste("\"", x@.col.name, "\"", sep = ""),
                .source = content(x),
                .parent = content(x),
                .conn.id = conn.id(x),
                .col.name = x@.col.name,
                .col.data_type = x@.col.data_type,
                .col.udt_name = x@.col.udt_name,
                .key = x@.key,
                .where = x@.where,
                .is.factor = TRUE,
                .sort = x@.sort)
        } else {
            new("db.Rquery",
                .content = x@.content,
                .expr = x@.expr,
                .source = x@.source,
                .parent = x@.parent,
                .conn.id = x@.conn.id,
                .col.name = x@.col.name,
                .col.data_type = x@.col.data_type,
                .col.udt_name = x@.col.udt_name,
                .key = x@.key,
                .where = x@.where,
                .is.factor = TRUE,
                .sort = x@.sort)
        }
    },
    valueClass = "db.Rquery")
