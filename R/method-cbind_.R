## method for cbind2

setMethod (
    "cbind2",
    signature (x = "db.obj", y = "db.obj"),
    def = function (x, y, ...) {
        z <- x
        for (name in names(y)) {
            if (! (name %in% names(z)))
                z[[name]] <- y[[name]]
            else {
                count <- 1
                while (paste0(name, ".", count) %in% names(z))
                    count <- count + 1
                z[[paste0(name, ".", count)]] <- y[[name]]
            }
        }
        z
    },
    valueClass = "db.Rquery")
