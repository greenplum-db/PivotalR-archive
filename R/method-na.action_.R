## ----------------------------------------------------------------------
## na.actions methods
## inplement in R space
## ----------------------------------------------------------------------

setGeneric("na.omit", function(object, ...) standardGeneric("na.omit"))

setMethod(
    "na.omit",
    signature(object = "db.obj"),
    def = function(object, vars = NULL, ...)
{
    x <- object
    if (is.null(vars)) {
        for (col in names(x)) x <- x[!is.na(x[[col]]),]
        x
    } else {
        warn.r <- getOption("warn")
        options(warn = -1)

        cond <- .row.action(.combine.list(Filter(function (v) !is.null(v),
                                                 Map(function (v)
                                                     eval(parse(text = paste(
                                                                "with(object, is.na(",
                                                                gsub("\"", "`", v),
                                                                "))",
                                                                sep = ""))),
                                                     vars))), " or ")
        
        options(warn = warn.r)
        x[!(cond),]
    }
})
