
## ------------------------------------------------------------------------
## several aggregate methods, can be used with by
## ------------------------------------------------------------------------

.aggregate <- function (x, func, vector = TRUE,
                        data.type = "double precision",
                        udt.name = "float8")
{
    if (vector && length(names(x)) != 1)
        stop(func, " only works on a single column!")

    if (vector == FALSE) {
        if (length(data.type) == 1)
            data.type <- rep(data.type, length(names(x)))
        if (length(udt.name) == 1)
            udt.name <- rep(udt.name, length(names(x)))
        if (length(data.type) != length(names(x)) ||
            length(udt.name) != length(names(x)))
            stop("data.type or udt_name does not match column names!")
    }
    
    if (is(x, "db.data.frame")) {
        expr <- paste(func, "(", names(x), ")", sep = "")
        parent <- content(x)
        src <- parent
        where.str <- ""
        where <- ""
    } else {
        expr <- paste(func, "(", x@.expr, ")", sep = "")
        parent <- x@.parent
        src <- x@.source
        where <- x@.where
        if (x@.where != "")
            where.str <- paste("where", x@.where)
        else
            where.str <- ""
    }
    
    col.name <- paste(names(x), "_", func, sep = "")
    
    content <- paste("select", paste(expr, col.name,
                                     sep = " as ",
                                     collapse = ", "),
                     "from", parent,
                     where)
    
    new("db.Rquery",
        .content = content,
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = data.type,
        .col.udt_name = udt.name,
        .where = where,
        .is.factor = rep(FALSE, length(names(x))),
        .sort = list(by = "", order = ""))
}

## ------------------------------------------------------------------------

setGeneric ("mean")

setMethod (
    "mean",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "avg", TRUE, "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("sum")

setMethod (
    "sum",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "sum", TRUE, x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("length")

setMethod (
    "length",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "count", TRUE, "integer", "int4")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("max")

setMethod (
    "max",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "max", TRUE, x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("min")

setMethod (
    "min",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "min", TRUE, x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("sd")

setMethod (
    "sd",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "stddev", TRUE, "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("var")

setMethod (
    "var",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "variance", TRUE, "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("colMeans")

setMethod (
    "colMeans",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "avg", FALSE, "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("colSums")

setMethod (
    "colSums",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "sum", FALSE, x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("c")

setMethod (
    "c",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "array_agg", FALSE, "ARRAY",
                   paste("_", x@.col.udt_name, sep = ""))
    },
    valueClass = "db.Rquery")

