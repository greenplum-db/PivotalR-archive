
## -----------------------------------------------------------------------
## print formating
## -----------------------------------------------------------------------

## maximum string width in a vector strings
.max.width <- function (strvec)
{
    smax <- 0
    for (i in strvec)
        if (nchar(i) > smax)
            smax <- nchar(i)
    smax
}

## -----------------------------------------------------------------------

## print a number of white spaces
.print.spaces <- function (n = 1)
{
    for (i in seq_len(n)) cat(" ")
}

## -----------------------------------------------------------------------

## generate sort and sort.str
.generate.sort <- function (x)
{
    if (is(x, "db.data.frame")) {
        sort <- list(by = "", order = "", str = "")
    } else {
        sort <- x@.sort
    }
    ## if (sort$by != "")
    ##     sort.str <- paste(" order by ",
    ##                       paste("\"", sort$by, "\"",
    ##                             collapse = ", ", sep = ""),
    ##                       sort$order, sep = "")
    ## else
    ##     sort.str <- ""
    sort
}
