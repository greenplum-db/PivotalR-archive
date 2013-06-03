
## ------------------------------------------------------------------------
## Preview the object
## ------------------------------------------------------------------------

setGeneric (
    "preview",
    def = function (x, ...) standardGeneric("preview"),
    signature = "x")

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.table"),
    def = function (x, nrows = 100) {
        .db.getQuery(paste("select * from", content(x), "limit", nrows),
                     conn.id(x))
    })

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.view"),
    def = function (x, nrows = 100, interactive = FALSE) {
        if (interactive) {
            cat(deparse(substitute(x)),
                "points to a view in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        .db.getQuery(paste("select * from", content(x), "limit", nrows),
                     conn.id(x))
    })

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rquery"),
    def = function (x, nrows = 100, interactive = FALSE) {
        msg.level <- .set.msg.level("panic", conn.id(x)) # suppress all messages
        warn.r <- getOption("warn")
        options(warn = -1)

        if (interactive) {
            cat(deparse(substitute(x)),
                "is just a query in R and does not point to any object in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        res <- .db.getQuery(paste(content(x), "limit", nrows),
                            conn.id(x))

        msg.level <- .set.msg.level(msg.level, conn.id(x)) # reset message level
        options(warn = warn.r) # reset R warning level
           
        return (res)
    })
