
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

setMethod ("==",
           c("db.obj", "db.obj"),
           function (e1, e2) {
               if (class(e1)[1] != class(e2)[1])
                   return (FALSE)
               if (is(e1, "db.data.frame")) {
                   if (all(e1@.name == e1@.name) &&
                       e1@.content == e2@.content &&
                       conn.eql(e1@.conn.id, e2@.conn.id) &&
                       e1@.table.type == e2@.table.type)
                       return (TRUE)
                   else
                       return (FALSE)
               } else {
                   if (e1@.content == e2@.content &&
                       all(e1@.expr == e1@.expr) &&
                       e1@.parent == e2@.parent &&
                       conn.eql(e1@.conn.id, e2@.conn.id) &&
                       all(e1@.names == e2@.names))
                       return (TRUE)
                   else
                       return (FALSE)
               }
           },
           valueClass = "logical")

setMethod ("!=",
           c("db.obj", "db.obj"),
           function (e1, e2) {
               if (e1 == e2)
                   FALSE
               else
                   TRUE
           },
           valueClass = "logical")

## ------------------------------------------------------------------------

.compare <- function(e1, e2, cmp, data.types,
                     prefix = "", res.type = "boolean")
{
    expr <- rep("", length(names(e1)))
    col.data_type <- rep("", length(names(e1)))
    col.udt_name <- rep("", length(names(e1)))
    col.name <- rep("", length(names(e1)))
    for (i in seq_len(length(names(e1)))) {
        if (e1@.col.data_type[i] %in% data.types) {
            expr[i] <- paste(prefix, e1@.expr[i], cmp, e2)
            col.data_type[i] <- res.type
            col.udt_name[i] <- res.type
        } else {
            expr[i] <- "NULL"
            col.data_type[i] <- "NULL"
            col.udt_name[i] <- "NULL"
        }
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
    
    expr.str <- paste(expr, col.name, sep = " as ", collapse = ", ")
    new("db.Rquery",
        .content = paste("select", expr.str, "from", tbl),
        .expr = expr,
        .source = e1@.source,
        .parent = e1@.parent,
        .conn.id = conn.id(e1),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = e1@.where,
        .is.factor = rep(FALSE, length(col.name)))
}

## ------------------------------------------------------------------------

.num.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint", "decimal", "numeric", "double precision",
                "float8", "real", "serial", "bigserial")

.int.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint")

.txt.types <- c("character varying", "varchar", "character",
                "char", "text")

## ------------------------------------------------------------------------

setMethod(
    ">",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, ">", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "<",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 > e1
    },
    valueClass = "db.Rquery")

setMethod(
    "<",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "<", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    ">",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 < e1
    },
    valueClass = "db.Rquery")

setMethod(
    ">=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, ">=", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "<=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 >= e1
    },
    valueClass = "db.Rquery")

setMethod(
    "<=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "<=", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    ">=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 <= e1
    },
    valueClass = "db.Rquery")

setMethod(
    "==",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "==", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "==",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 == e1
    },
    valueClass = "db.Rquery")

setMethod(
    "!=",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "!=", .num.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "!=",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 != e1
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod(
    ">",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, ">", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "<",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 > e1
    },
    valueClass = "db.Rquery")

setMethod(
    "<",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, "<", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    ">",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 < e1
    },
    valueClass = "db.Rquery")

setMethod(
    ">=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, ">=", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "<=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 >= e1
    },
    valueClass = "db.Rquery")

setMethod(
    "<=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, "<=", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    ">=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 <= e1
    },
    valueClass = "db.Rquery")

setMethod(
    "==",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, "==", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "==",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 == e1
    },
    valueClass = "db.Rquery")

setMethod(
    "!=",
    signature(e1 = "db.obj", e2 = "character"),
    function (e1, e2) {
        .compare(e1, e2, "!=", .txt.types)
    },
    valueClass = "db.Rquery")

setMethod(
    "!=",
    signature(e1 = "character", e2 = "db.obj"),
    function (e1, e2) {
        e2 != e1
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod(
    "+",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "+", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "+",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 + e1
    },
    valueClass = "db.Rquery")

setMethod(
    "-",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "-", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "-",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, e1, "+", .num.types, prefix = "-",
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "*",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "*", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "*",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        e2 * e1
    },
    valueClass = "db.Rquery")

setMethod(
    "/",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "/", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "/",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, e1, "*", .num.types, prefix = "1./",
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "^",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "^", .num.types, res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "^",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "^", sep = ""),
                 res.type = "double precision")
    },
    valueClass = "db.Rquery")

setMethod(
    "%%",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, e2, "%", .num.types, res.type = "integer")
    },
    valueClass = "db.Rquery")

setMethod(
    "%%",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "%"),
                 res.type = "integer")
    },
    valueClass = "db.Rquery")

setMethod(
    "%/%",
    signature(e1 = "db.obj", e2 = "numeric"),
    function (e1, e2) {
        .compare(e1, as.integer(e2), "/", .int.types, res.type = "integer")
    },
    valueClass = "db.Rquery")

setMethod(
    "%/%",
    signature(e1 = "numeric", e2 = "db.obj"),
    function (e1, e2) {
        .compare(e2, "", "", .num.types,
                 prefix = paste(e1, "%"),
                 res.type = "integer")
    },
    valueClass = "db.Rquery")


