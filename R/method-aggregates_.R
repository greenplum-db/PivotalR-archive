
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

## array_agg each column
colAgg <- function (x)
{
    if (!is(x, "db.obj"))
        stop("this function only works with db.obj!")
    .aggregate(x, "array_agg", FALSE, NULL, FALSE, "ARRAY",
               paste("_", x@.col.udt_name, sep = ""))
}

## ------------------------------------------------------------------------

## array_agg all the columns on the same row
rowAgg <- function (x, ...)
{
    n <- nargs()
    if (!is(x, "db.obj")) stop("this function only works with db.obj!")
    if (!all(x@.col.data_type %in% .num.types) ||
        !all(x@.col.data_type %in% .txt.types) ||
        !all(x@.col.data_type %in% c('boolean')))
        stop("columns cannot be put into the same array!")
    for (i in seq_len(n-1)) {
        y <- eval(parse(text = paste0("..", i)))
        if (!is(y, "db.obj")) stop("this function only works with db.obj!")
        if (!.eql.parent(x, y))
            stop("all the objects must originate from the same source!")
        data.types <- c(x@.col.data_type[1], y@.col.data_type)
        if (!all(data.types %in% .num.types) ||
            !all(data.types %in% .txt.types) ||
            !all(data.types %in% c('boolean')))
            stop("columns cannot be put into the same array!")
    }

    if (x@.col.data_type %in% .num.types)
        udt.name <- "_float8"
    else if (x@.col.data_type %in% .txt.types)
        udt.name <- "_text"
    else
        udt.name <- "_bool"
    
    if (is(x, "db.data.frame")) {
        tbl <- content(x)
        src <- tbl
        parent <- tbl
    } else {
        if (x@.parent != x@.source)
            tbl <- paste("(", x@.parent, ") s", sep = "")
        else
            tbl <- x@.parent
        parent <- x@.parent
        src <- x@.source
    }

    if (is(x, "db.Rquery") && x@.where != "") {
        where.str <- paste(" where", x@.where)
        where <- x@.where
    } else {
        where.str <- ""
        where <- ""
    }

    sort <- .generate.sort(x)

    expr <- "array["
    if (is(x, "db.data.frame"))
        expr <- paste(expr, paste(names(x), collapse = ", "), sep = "")
    else
        expr <- paste(expr, paste(x@.expr, collapse = ", "), sep = "")
    for (i in seq_len(n-1)) {
        y <- eval(parse(text = paste0("..", i)))
        if (is(y, "db.data.frame"))
            expr <- paste(expr, paste(names(y), collapse = ", "), sep = "")
        else
            expr <- paste(expr, paste(y@.expr, collapse = ", "), sep = "")
    }
    expr <- paste(expr, "]", sep = "")

    new("db.Rquery",
        .content = paste("select ", expr, " as agg_opr from ",
        tbl, where.str, sort$str, sep = ""),
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = "agg_opr",
        .key = character(0),
        .where = "",
        .col.data_type = "array",
        .col.udt_name = udt.name,
        .is.factor = FALSE,
        .factor.suffix = "",
        .sort = sort)
}

## ------------------------------------------------------------------------

.eql.parent <- function (x1, x2)
{
    if (is(x1, "db.data.frame") && is(x2, "db.data.frame")) {
        return (content(x1) == content(x2) &&
                conn.eql(conn.id(x1), conn.id(x2)))
    } else if (is(x1, "db.data.frame") && is(x2, "db.Rquery")) {
        return (content(x1) == x2@.parent && x2@.where = "" &&
                conn.eql(conn.id(x1), conn.id(x2)))
    } else if (is(x1, "db.Rquery") && is(x2, "db.data.frame"))
        .eql.parent(x2, x1)
    else {
        return (x1@.parent == x2@.parent && x1@.where = x2@.where &&
                conn.eql(conn.id(x1), conn.id(x2)))
    }
}
