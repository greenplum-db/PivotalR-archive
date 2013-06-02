
## ------------------------------------------------------------------------
## Some operations for Arith and Compare
## 6 Compare operation methods, 7 Arith operation methods
##
## For each operation, we need to create methods for signatures
## db.data.frame and db.data.frame
## db.Rquery and db.Rquery,
## db.Rquery and numeric, numeric and db.Rquery
## db.Rquery and character, character and db.Rquery
##
## That would be 13 x 6 = 78 methods! How to avoid typing so many
## functions? But of course, they are small functions, and can be
## done in an hour. However, this would bring lots of redundant
## things into the manual.
## ------------------------------------------------------------------------

setGeneric ("eql", signature = c("e1", "e2"),
            def = function (e1, e2) standardGeneric("eql"))

## --

## Test whether two object pointing to the same thing
setMethod (
    "eql",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        if (class(e1)[1] != class(e2)[1])
            return (FALSE)
        if (is(e1, "db.data.frame")) {
            if (all(e1@.name == e2@.name) &&
                e1@.content == e2@.content &&
                conn.eql(e1@.conn.id, e2@.conn.id) &&
                e1@.table.type == e2@.table.type)
                return (TRUE)
            else
                return (FALSE)
        } else {
            if (e1@.content == e2@.content &&
                length(e1@.expr) == length(e2@.expr) &&
                all(e1@.expr == e2@.expr) &&
                e1@.source == e2@.source &&
                e1@.parent == e2@.parent &&
                conn.eql(e1@.conn.id, e2@.conn.id) &&
                all(e1@.col.data_type == e2@.col.data_type) &&
                .eql.where(e1@.where, e2@.where) &&
                all(e1@.is.factor == e2@.is.factor) &&
                all(e1@.col.name == e2@.col.name))
                return (TRUE)
            else
                return (FALSE)
        }
    },
    valueClass = "logical")

## ------------------------------------------------------------------------

## operation between an db.obj and a single value
.compare <- function (e1, e2, cmp, data.types,
                      prefix = "", res.type = "boolean",
                      cast = "::double precision")
{
    if (is(e1, "db.data.frame")) e1 <- e1[,]    
    expr <- rep("", length(names(e1)))
    col.data_type <- rep("", length(names(e1)))
    col.udt_name <- rep("", length(names(e1)))
    col.name <- rep("", length(names(e1)))
    for (i in seq_len(length(names(e1)))) {
        if (e1@.col.data_type[i] %in% data.types || is.na(data.types)) {
            expr[i] <- paste(prefix, "(", e1@.expr[i], ")", cast,
                             cmp, e2, sep = "")
        } else {
            expr[i] <- "NULL"
        }
        col.data_type[i] <- res.type
        col.udt_name[i] <- res.type
        col.name[i] <- paste(names(e1)[i], "_opr", sep = "")
    }
    
    if (is(e1, "db.data.frame"))
        tbl <- content(e1)
    else {
        if (e1@.source == e1@.parent)
            tbl <- e1@.source
        else
            tbl <- paste("(", e1@.parent, ")", sep = "")
    }

    if (e1@.where != "") where.str <- paste(" where", e1@.where)
    else where.str <- ""

    sort <- .generate.sort(e1)
    
    expr.str <- paste(expr, paste("\"", col.name, "\"", sep = ""),
                      sep = " as ", collapse = ", ")
    new("db.Rquery",
        .content = paste("select ", expr.str, " from ", tbl,
        where.str, sort$str, sep = ""),
        .expr = expr,
        .source = e1@.source,
        .parent = e1@.parent,
        .conn.id = conn.id(e1),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = e1@.where,
        .is.factor = rep(FALSE, length(col.name)),
        .sort = sort)
}

## ------------------------------------------------------------------------

.num.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint", "decimal", "numeric", "double precision",
                "float8", "real", "serial", "bigserial")

## --

.int.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint")

## --

.txt.types <- c("character varying", "varchar", "character",
                "char", "text")

## ------------------------------------------------------------------------

setMethod (
    ">",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " > ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 > e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " < ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 < e1
    },
    valueClass = "db.Rquery")

## --

setMethod(
    ">=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " >= ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod(
    "<=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 >= e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " <= ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 <= e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "==",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " = ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "==",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 == e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " <> ", .num.types)
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 != e1
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod (
    ">",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " > ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 > e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " < ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 < e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " >= ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 >= e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " <= ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 <= e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "==",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " = ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "==",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 == e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, " <> ", .txt.types, cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 != e1
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod (
    "&",
    signature(e1 = "db.obj", e2 = "logical"),
    function (e1, e2) {
        .compare(e1, e2, " and ", c("boolean"), res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "|",
    signature(e1 = "db.obj", e2 = "logical"),
    function (e1, e2) {
        .compare(e1, e2, " or ", c("boolean"), res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

setMethod (
    "&",
    signature(e1 = "logical", e2 = "db.obj"),
    function (e1, e2) {
        e2 & e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "|",
    signature(e1 = "logical", e2 = "db.obj"),
    function (e1, e2) {
        e2 | e1
    },
    valueClass = "db.Rquery")


## ------------------------------------------------------------------------

setMethod (
    "+",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " + ", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "+",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 + e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "-",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " - ", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "-",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, e1, " + ", .num.types, prefix = "-",
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "*",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " * ", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "*",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 * e1
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "/",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " / ", .num.types, res.type = "double precision", cast = "::double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "/",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, e1, " * ", .num.types, prefix = "1./",
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "^",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "^", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "^",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "^", sep = ""),
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%%",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, " % ", .num.types, res.type = "double precision", cast = "::integer")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%%",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "% "),
                 res.type = "double precision", cast = "::integer")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%/%",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, as.integer(e2), " / ", .int.types, res.type = "integer", cast = "::integer")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%/%",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "%"),
                 res.type = "integer", cast = "::integer")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

## Arith operators for db.Rquery and db.Rquery

## operator for two db.obj objects
.operate.two <- function (e1, e2, op, data.types, 
                          res.type = "boolean",
                          cast = "::double precision")
{
    ## convert db.data.frame into db.Rquery
    if (is(e1, "db.data.frame")) e1 <- e1[,]
    if (is(e2, "db.data.frame")) e2 <- e2[,]

    l1 <- length(names(e1))
    l2 <- length(names(e2))
    if (length(names(e1)) > length(names(e2)))
        l <- l1
    else
        l <- l2

    if (e1@.parent != e2@.parent || !.eql.where(e1@.where, e2@.where))
        stop("How can you match the rows of two objects",
             " if they are not derived from the same thing!")
    
    expr <- rep("", length(names(e1)))
    col.data_type <- rep("", length(names(e1)))
    col.udt_name <- rep("", length(names(e1)))
    col.name <- rep("", length(names(e1)))
    for (i in seq_len(l)) {
        i1 <- (i-1) %% l1 + 1
        i2 <- (i-1) %% l2 + 1
        v <- 0
        for (k in seq_len(length(data.types)))
            if (e1@.col.data_type[i1] %in% data.types[[k]]) {
                v <- k
                break
            }
        if (v > 0 && e2@.col.data_type[i2] %in% data.types[[v]]) {
            expr[i] <- paste("(", e1@.expr[i1], ")", cast, op, "(",
                             e2@.expr[i2], ")", sep = "")
        } else {
            expr[i] <- "NULL"
        }
        col.data_type[i] <- res.type
        col.udt_name[i] <- res.type
        col.name[i] <- paste(names(e1)[i1], "_", names(e2)[i2],
                             "_opr", sep = "")
    }
    
    if (e1@.source == e1@.parent)
        tbl <- e1@.source
    else
        tbl <- paste("(", e1@.parent, ")", sep = "")

    if (e1@.where != "") where.str <- paste(" where", e1@.where)
    else where.str <- ""

    sort <- .generate.sort(e1)
    
    expr.str <- paste(expr, paste("\"", col.name, "\"", sep = ""),
                      sep = " as ", collapse = ", ")
    new("db.Rquery",
        .content = paste("select ", expr.str, " from ", tbl,
        where.str, sort$str, sep = ""),
        .expr = expr,
        .source = e1@.source,
        .parent = e1@.parent,
        .conn.id = conn.id(e1),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = e1@.where,
        .is.factor = e1@.is.factor & e2@.is.factor,
        .sort = sort)
}

## ------------------------------------------------------------------------

setMethod (
    "+",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " + ", list(.num.types),
                     res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "-",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " - ", list(.num.types),
                     res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "*",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " * ", list(.num.types),
                     res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "/",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " / ", list(.num.types),
                     res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "^",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " ^ ", list(.num.types),
                     res.type = "double precision")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%%",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " % ", list(.num.types),
                     res.type = "double precision", cast = "::integer")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "%/%",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " / ", list(.int.types),
                     res.type = "integer", cast = "::integer")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " > ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " < ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    ">=",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " >= ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "<=",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " <= ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "==",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " = ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!=",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " <> ", list(.num.types, .txt.types),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "&",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " and ", list(c("boolean")),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "|",
    signature(e1 = "db.obj", e2 = "db.obj"),
    function (e1, e2) {
        .operate.two(e1, e2, " or ", list(c("boolean")),
                     res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## --

setMethod (
    "!",
    signature(x = "db.obj"),
    function (x) {
        .compare(x, "", "", c("boolean"), prefix = "not ",
                 res.type = "boolean", cast = "")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setGeneric ("is.na")

setMethod (
    "is.na",
    signature(x = "db.obj"),
    function (x) {
        .compare(x, "", " is NULL", NA, "", "boolean", "")
    },
    valueClass = "db.Rquery")
