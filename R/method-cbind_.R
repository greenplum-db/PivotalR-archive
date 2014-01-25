## method for cbind2

setMethod (
    "cbind2",
    signature (x = "db.obj", y = "db.obj"),
    def = function (x, y) {
        z <- x
        for (name in names(y)) {
            if (! (name %in% names(z)))
                z[[name]] <- y[[name]]
            else {
                count <- 1
                while (paste(name, ".", count, sep = "") %in% names(z))
                    count <- count + 1
                z[[paste(name, ".", count, sep = "")]] <- y[[name]]
            }
        }
        z
    },
    valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setGeneric("cbind", function(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`, ..., deparse.level = 1) {
    if (missing(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`) && !is(..1, "db.obj")
        || (!missing(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`) && !is(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`, "db.obj"))) {
        if (missing(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`))
            args <- list(..., deparse.level = deparse.level)
        else
            args <- list(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`, ..., deparse.level = deparse.level)
        do.call(base::cbind, args)
    } else {
        if (missing(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`))
            lst <- c(...)
        else
            lst <- c(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`, ...)
        .combine.list(lst)
    }
})

