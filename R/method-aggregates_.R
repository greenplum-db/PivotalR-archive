
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
    base <- NULL
    if (is(x, "db.obj")) base <- x
    else
        for (i in seq_len(n-1)) {
            y <- eval(parse(text = paste0("..", i)))
            if (is(y, "db.obj")) {
                base <- y
                break
            }
        }
    if (is.null(base)) return (c(x, ...))

    if (!all(base@.col.data_type %in% .num.types) &&
        !all(base@.col.data_type %in% .txt.types) &&
        !all(base@.col.data_type %in% c('boolean')))
        stop("columns cannot be put into the same array!")

    if (base@.col.data_type[1] %in% .num.types)
        udt.name <- "_float8"
    else if (base@.col.data_type[1] %in% .txt.types)
        udt.name <- "_text"
    else if (base@.col.data_type[1] %in% c("boolean")) 
        udt.name <- "_bool"
    else
        stop("data type not supported!")
    
    if (is(base, "db.data.frame")) {
        tbl <- content(base)
        src <- tbl
        parent <- tbl
    } else {
        if (base@.parent != base@.source)
            tbl <- paste("(", base@.parent, ") s", sep = "")
        else
            tbl <- base@.parent
        parent <- base@.parent
        src <- base@.source
    }

    if (is(base, "db.Rquery") && base@.where != "") {
        where.str <- paste(" where", base@.where)
        where <- base@.where
    } else {
        where.str <- ""
        where <- ""
    }

    sort <- .generate.sort(base)

    ## if (!is(base, "db.obj")) stop("this function only works with db.obj!")
    expr <- "array["
    x <- .check.consistent(base, x, udt.name)
    if (is(x, "db.data.frame"))
        expr <- paste(expr, paste("(\"", names(x), "\")", sep = "",
                                  collapse = ", "), sep = "")
    else if (is(x, "db.Rquery"))
        expr <- paste(expr, paste("(", x@.expr, ")", sep = "",
                                  collapse = ", "), sep = "")
    else
        expr <- paste(expr, paste(x, collapse = ", "), sep = "")
    if (n > 1) expr <- paste(expr, ", ", sep = "")
    for (i in seq_len(n-1)) {
        y <- eval(parse(text = paste0("..", i)))
        ## if (!is(y, "db.obj")) stop("this function only works with db.obj!")
        y <- .check.consistent(base, y, udt.name)
        if (is(y, "db.data.frame"))
            expr <- paste(expr, paste("(\"", names(y), "\")", sep = "",
                                      collapse = ", "), sep = "")
        else if (is(y, "db.Rquery"))
            expr <- paste(expr, paste("(", y@.expr, ")", sep = "",
                                      collapse = ", "), sep = "")
        else
            expr <- paste(expr, paste(y, collapse = ", "), sep = "")
        if (i != n-1) expr <- paste(expr, ", ", sep = "")
    }
    expr <- paste(expr, "]", sep = "")

    new("db.Rquery",
        .content = paste("select ", expr, " as agg_opr from ",
        tbl, where.str, sort$str, sep = ""),
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(base),
        .col.name = "agg_opr",
        .key = character(0),
        .where = where,
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
        return (content(x1) == x2@.parent && x2@.where == "" &&
                conn.eql(conn.id(x1), conn.id(x2)))
    } else if (is(x1, "db.Rquery") && is(x2, "db.data.frame"))
        .eql.parent(x2, x1)
    else {
        return (x1@.parent == x2@.parent && x1@.where == x2@.where &&
                conn.eql(conn.id(x1), conn.id(x2)))
    }
}

## ------------------------------------------------------------------------

.check.consistent <- function (base, x, udt.name)
{
    if (is(x, "db.obj")) {
        if (!.eql.parent(base, x))
            stop("all the objects must originate from the same source!")
        data.types <- c(base@.col.data_type[1], x@.col.data_type)
        if (!all(data.types %in% .num.types) &&
            !all(data.types %in% .txt.types) &&
            !all(data.types %in% c('boolean')))
            stop("columns cannot be put into the same array!")
    } else {
        if (udt.name == "_float") x <- as.numeric(x)
        else if (udt.name == "_text") x <- as.character(x)
        else if (udt.name == "_bool") x <- as.logical(x)
    }
    x
}
