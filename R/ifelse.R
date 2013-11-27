## ----------------------------------------------------------------------
## ifelse
## ----------------------------------------------------------------------

setGeneric("ifelse")

setMethod("ifelse",
    signature(test = "db.obj"),
    function (test, yes, no)
    {
        if (length(names(test)) != 1 || col.types(test) != "boolean")
            stop(deparse(substitue(test)), " must have only one column",
                 " and the column type is boolean!")
        x <- test
        x$res <- no
        x$res[test,] <- yes
        x$res
    }
)
