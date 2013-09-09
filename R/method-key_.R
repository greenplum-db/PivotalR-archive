
## -----------------------------------------------------------------------
## get or set key
## -----------------------------------------------------------------------

key <- function (x)
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")
    x@.key   
}

## -----------------------------------------------------------------------

"key<-" <- function (x, value = character(0))
{
    if (! is(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")

    if (!is.character(value))
        stop("key must be a column name, which is a string!")

    if (!value %in% names(x))
        stop(deparse(substitute(x)), " does not have a column named ",
             value)
    
    x@.key <- value
    x
}
