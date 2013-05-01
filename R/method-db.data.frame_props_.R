
setMethod (
    "dim",
    signature(x = "db.table"),
    function (x) {
        x@.dim
    })

## ------------------------------------------------------------------------

setMethod (
    "names",
    signature(x = "db.data.frame"),
    function (x) {
        x@.col.name
    })

## ------------------------------------------------------------------------

content <- function (x)
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")

    x@.content
}

## ------------------------------------------------------------------------

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

