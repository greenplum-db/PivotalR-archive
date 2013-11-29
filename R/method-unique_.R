
## -----------------------------------------------------------------------
## unique method
## -----------------------------------------------------------------------

setGeneric ("unique")

## have different returns for array
setMethod(
    "unique",
    signature(x = "db.obj"),
    function (x, incomparables = FALSE, ...) {
        if (length(names(x)) != 1)
            stop("unique can only work on objects with only one column!")

        res <- x
        res@.expr <- paste("distinct ", x@.expr, sep = "")
        ## res@.content <- gsub("^select [^((?! as ).)]+\\S+ as",
        res@.content <- gsub("^select .* as",
                             paste("select ", res@.expr, " as", sep = ""),
                             x@.content)
        return (res)
    })
