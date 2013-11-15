## ----------------------------------------------------------------------
## na.actions methods
## inplement in R space
## ----------------------------------------------------------------------

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

        cond <- Reduce(function (v, w) v | w,
                       Filter(function (v) !is.null(v),
                              unlist(Map(function (v)
                                         eval(parse(text = paste(
                                                    "with(object, is.na(",
                                                    gsub("\"", "`", v), "))",
                                                    sep = ""))),
                                         vars))))
        
        options(warn = warn.r)
        x[!(cond),]
    }
})
