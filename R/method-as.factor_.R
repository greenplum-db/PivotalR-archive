
## -----------------------------------------------------------------------
## as.factor
## -----------------------------------------------------------------------

setGeneric ("as.factor")

setMethod (
    "as.factor",
    signature(x = "db.obj"),
    function (x) {
        if (length(x@.col.name) != 1)
            stop("Cannot coerce multiple columns into factor!")
        if (x@.col.data_type == "array")
            stop("Cannot set an array to be a factor!")
        if (x@.is.factor) return (x)
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
                .factor.suffix = .unique.string.short(),
                .sort = x@.sort,
                .dist.by = x@.dist.by)
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
                .factor.ref = as.character(NA),
                .factor.suffix = .unique.string.short(),
                .sort = x@.sort,
                .dist.by = x@.dist.by)
        }
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------

setGeneric ("is.factor")

setMethod (
    "is.factor",
    signature(x = "db.obj"),
    function (x) {
        if (all(x@.is.factor == TRUE))
            TRUE
        else
            FALSE
    })

## ----------------------------------------------------------------------

setGeneric ("relevel")

setMethod(
    "relevel",
    signature(x = "db.obj"),
    function(x, ref, ...) {
        if (length(x@.col.name) != 1)
            stop("Cannot relevel multiple columns!")
        if (!x@.is.factor) x <- as.factor(x)
        x@.factor.ref <- (if (is.null(ref) || is.na(ref))
                          .null.string else as.character(ref))
        x
    })
