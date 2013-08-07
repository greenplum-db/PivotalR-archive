## method for cbind2

setMethod (
    "cbind2",
    signature (x = "db.obj", y = "db.obj"),
    def = function (x, y, ...) {
        z <- x
        for (name in names(y))
            z[[name]] <- y[[name]]
        z
    },
    valueClass = "db.Rquery")
