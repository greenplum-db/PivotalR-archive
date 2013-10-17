
.attach.cast <- function(x, cast, udt)
{
    x <- .expand.array(x)
    for (i in seq_len(length(names(x)))) {
        if (x@.col.data_type[i] == "array") {
            x@.expr[i] <- paste("(", x@.expr[i], ")::", cast, "[]",
                                sep = "")
            x@.col.udt_name[i] <- paste("_", udt, sep = "")
        } else {
            x@.expr[i] <- paste("(", x@.expr[i], ")::", cast,
                                sep = "")
            x@.col.data_type[i] <- cast
            x@.col.udt_name[i] <- udt
        }
    }
    if (x@.where != "") where.str <- paste(" where", x@.where)
    else where.str <- ""
    i.str <- paste(x@.expr, paste("\"", x@.col.name, "\"", sep = ""),
                   sep = " as ", collapse = ", ")
    if (x@.parent == x@.source)
        tbl <- x@.parent
    else
        tbl <- paste("(", x@.parent, ") s", sep = "")
    x@.content <- paste("select ", i.str, " from ", tbl, where.str,
                        x@.sort$str, sep = "")
    x
}

## ----------------------------------------------------------------------

setMethod(
    "as.integer",
    signature(x = "db.obj"),
    def = function(x, ...)
{
    .attach.cast(x, "integer", "int4")
})

## ----------------------------------------------------------------------

setMethod(
    "as.character",
    signature(x = "db.obj"),
    def = function(x, ...)
{
    .attach.cast(x, "text", "text")
})

## ----------------------------------------------------------------------

setGeneric("as.double")

setMethod(
    "as.double",
    signature(x = "db.obj"),
    def = function(x, ...)
{
    .attach.cast(x, "double precision", "float8")
})

## ----------------------------------------------------------------------

setMethod(
    "as.logical",
    signature(x = "db.obj"),
    def = function(x, ...)
{
    .attach.cast(x, "boolean", "bool")
})

## ----------------------------------------------------------------------

setMethod(
    "as.numeric",
    signature(x = "db.obj"),
    def = function(x, ...)
{
    .attach.cast(x, "double precision", "float8")
})
