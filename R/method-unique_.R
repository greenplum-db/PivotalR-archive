
## -----------------------------------------------------------------------
## unique method
## -----------------------------------------------------------------------

setGeneric ("unique")

## have different returns for array
setMethod(
    "unique",
    signature(x = "db.obj"),
    function (x, incomparables = FALSE, ...) {
        res <- .aggregate(x, "array_agg", TRUE, NULL, FALSE, "array",
                          paste("_", x@.col.udt_name, sep = ""),
                          inside = "distinct ")
        
        if (length(names(x)) == 1 && x@.col.data_type == "array") {
            expr <- paste("distinct ", x@.expr, sep = "")
            res@.expr <- expr
            res@.col.udt_name <- gsub("^_", "", res@.col.udt_name)
            res@.content <- gsub("^select (.*) as", paste("select ",
                                                          expr, " as",
                                                          sep = ""),
                                 res@.content)
        }        

        return (res)
    })
