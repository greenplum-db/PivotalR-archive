
## ------------------------------------------------------------------------
## several aggregate methods, can be used with by
## ------------------------------------------------------------------------

.aggregate <- function (x, func, vector = TRUE, input.types = .num.types,
                        allow.bool = FALSE,
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

    cast.bool <- rep("", length(names(x)))
    if (allow.bool)
        for (i in seq_len(length(names(x))))
            if (x@.col.data_type == "boolean") cast.bool[i] <- "::integer"
    
    if (is(x, "db.data.frame")) {
        expr <- paste(func, "(\"", names(x), "\"", cast.bool, ")",
                      sep = "")
        if (!is.null(input.types)) {
            for (i in seq_len(length(expr)))
                if (!(x@.col.data_type[i] %in% input.types)) {
                    if (allow.bool && x@.col.data_type == "boolean") next
                    expr[i] <-paste( "NULL::", x@.col.data_type[i], sep = "")
                    data.type[i] <- x@.col.data_type[i]
                    udt.name[i] <- x@.col.udt_name[i]
                }
        }
        parent <- content(x)
        src <- parent
        where.str <- ""
        where <- ""
    } else {
        expr <- paste(func, "((", x@.expr, ")", cast.bool, ")", sep = "")
        if (!is.null(input.types)) {
            for (i in seq_len(length(expr)))
                if (! (x@.col.data_type[i] %in% input.types)) {
                    if (allow.bool && x@.col.data_type == "boolean") next
                    expr[i] <- paste("NULL::", x@.col.data_type[i], sep = "")
                    data.type[i] <- x@.col.data_type[i]
                    udt.name[i] <- x@.col.udt_name[i]
                }
        }
        if (x@.source == x@.parent)
            parent <- x@.parent
        else
            parent <- paste("(", x@.parent, ") s", sep = "")
        src <- x@.source
        where <- x@.where
        if (x@.where != "")
            where.str <- paste(" where", x@.where)
        else
            where.str <- ""
    }

    col.name <- paste(names(x), "_", func, sep = "")

    content <- paste("select ", paste(expr,
                                     paste("\"", col.name, "\"", sep = ""),
                                     sep = " as ",
                                     collapse = ", "),
                     " from ", parent,
                     where.str, sep = "")

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
        .factor.suffix = rep("", length(names(x))),
        .sort = list(by = "", order = "", str = ""))
}

## ------------------------------------------------------------------------

setGeneric ("mean")

setMethod (
    "mean",
    signature(x = "db.obj"),
    function (x, ...) {
        .aggregate(x, "avg", TRUE, .num.types, TRUE,
                   "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("sum")

setMethod (
    "sum",
    signature(x = "db.obj"),
    function (x, ..., na.rm = FALSE) {
        .aggregate(x, "sum", TRUE, .num.types, TRUE,
                   x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("length")

setMethod (
    "length",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "count", TRUE, NULL, FALSE, "integer", "int4")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("max")

setMethod (
    "max",
    signature(x = "db.obj"),
    function (x, ..., na.rm = FALSE) {
        .aggregate(x, "max", TRUE, .num.types, FALSE,
                   x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("min")

setMethod (
    "min",
    signature(x = "db.obj"),
    function (x, ..., na.rm = FALSE) {
        .aggregate(x, "min", TRUE, .num.types, FALSE,
                   x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("sd", signature = "x",
            def = function (x, na.rm = FALSE) {
                if (!is(x, "db.obj")) {
                    stats::sd(x, na.rm)
                } else {
                    standardGeneric("sd")
                }
            })

setMethod (
    "sd",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "stddev", TRUE, .num.types, TRUE,
                   "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("var", signature = "x",
            def = function (x, y = NULL, na.rm = FALSE, use) {
                if (!is(x, "db.obj")) {
                    stats::var(x, y, na.rm, use)
                } else {
                    standardGeneric("var")
                }
            })

setMethod (
    "var",
    signature(x = "db.obj"),
    function (x) {
        .aggregate(x, "variance", TRUE, .num.types, TRUE,
                   "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("colMeans")

setMethod (
    "colMeans",
    signature(x = "db.obj"),
    function (x, na.rm = FALSE, dims = 1, ...) {
        .aggregate(x, "avg", FALSE, .num.types, TRUE,
                   "double precision", "float8")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("colSums")

setMethod (
    "colSums",
    signature(x = "db.obj"),
    function (x, na.rm = FALSE, dims = 1, ...) {
        .aggregate(x, "sum", FALSE, .num.types, TRUE,
                   x@.col.data_type, x@.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("c")

setMethod (
    "c",
    signature(x = "db.obj"),
    function (x, ..., recursive = FALSE) {
        .aggregate(x, "array_agg", TRUE, NULL, FALSE, "ARRAY",
                   paste("_", x@.col.udt_name, sep = ""))
    },
    valueClass = "db.Rquery")

