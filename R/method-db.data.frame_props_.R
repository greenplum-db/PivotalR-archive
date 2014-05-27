
setMethod (
    "dim",
    signature(x = "db.table"),
    function (x) {
        ## if (!is(x, "db.table"))
        ##     stop("Dim information is only available for db.table object!")
        x@.dim
    })

## ----------------------------------------------------------------------

setMethod (
    "dim",
    signature(x = "db.view"),
    function (x) {
        ncol <- length(names(x))
        nrow <- .db.getQuery(paste("select count(*) from ", content(x),
                             sep = ""), conn.id(x))[[1]]
        c(nrow, ncol)
    })

## ----------------------------------------------------------------------

setMethod (
    "dim",
    signature(x = "db.Rquery"),
    function (x) {
        ncol <- length(names(x))
        nrow <- .db.getQuery(paste("select count(*) from (", content(x), ") s",
                             sep = ""), conn.id(x))[[1]]
        c(nrow, ncol)
    })

## ----------------------------------------------------------------------

setMethod (
    "names",
    signature(x = "db.obj"),
    function (x) {
        x@.col.name
    })

## ----------------------------------------------------------------------

setMethod (
    "names<-",
    signature(x = "db.obj"),
    function (x, value) {
        if (!is.character(value))
            stop("The value to replace names is not correct!")
        l1 <- length(names(x))
        l2 <- length(value)
        if (l1 < l2) l <- l1
        else l <- l2
        if (l == 0)
            stop("The length of value is not correct!")
        select <- seq_len(l)

        if (is(x, "db.data.frame")) {
            expr <- x@.col.name[select]
            where <- ""
            where.str <- ""
            parent <- content(x)
            src <- parent
        } else {
            expr <- x@.expr[select]
            where <- x@.where
            if (where != "") where.str <- paste(" where", x@.where)
            else where.str <- ""
            parent <- x@.parent
            src <- x@.source
        }
        sort <- .generate.sort(x)
        sql <- paste("select ", paste(expr, paste("\"", value[select], "\"", sep = ""),
                                      sep = " as ",
                                      collapse = ", "),
                     " from ", parent, where.str, sort$str, sep = "")
        new("db.Rquery",
            .content = sql,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(x),
            .col.name = value[select],
            .key = key(x),
            .col.data_type = x@.col.data_type[select],
            .col.udt_name = x@.col.udt_name[select],
            .where = where,
            .is.factor = x@.is.factor[select],
            .factor.ref = x@.factor.ref[select],
            .factor.suffix = x@.factor.suffix[select],
            .sort = sort,
            .dist.by = x@.dist.by)
    },
    valueClass = "db.Rquery")

## ----------------------------------------------------------------------

content <- function (x)
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")

    x@.content
}

## ----------------------------------------------------------------------

conn.id <- function (x)
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")
    x@.conn.id
}

"conn.id<-" <- function (x, value = 1)
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")
    if (! .is.conn.id.valid(value))
        stop("There is no such a connection!")
    x@.conn.id <- value
    x
}

