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

setGeneric("cbind", function(x, ..., deparse.level = 1) {
    if (!is(x, "db.obj"))
        base::cbind(x, ..., deparse.level = 1)
    else
        standardGeneric("cbind")
})

setMethod(
    "cbind",
    representation(x = "db.obj"),
    function(x, ..., deparse.level = 1) {
        lst <- c(x, ...)
        PivotalR:::.combine.list(lst)
    })
