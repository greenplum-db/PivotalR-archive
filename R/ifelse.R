## ----------------------------------------------------------------------
## ifelse
## ----------------------------------------------------------------------

setGeneric("ifelse")

setMethod("ifelse",
    signature(test = "db.obj"),
    function (test, yes, no)
    {
        test <- test[,]
        if (length(names(test)) != 1)
            stop(deparse(substitute(test)), " must have only one column",
                 " and the column type can be casted into boolean!")
        x <- test
        x$res <- no
        x$res[as.logical(test),] <- yes
        x$res
    }
)
