
## ------------------------------------------------------------------------
## Preview the object
## ------------------------------------------------------------------------

setGeneric (
    "preview",
    def = function (x, ...) standardGeneric("preview"),
    signature = "x")

## ------------------------------------------------------------------------

.limit.str <- function (nrows)
{
    if (is.null(nrows) || (is.character(nrows) && nrows == "all"))
        limit.str <- ""
    else if (is.numeric(nrows))
        limit.str <- paste(" limit ", nrows, sep = "")
    else
        stop("nrows must be NULL, \"all\" or an integer!")
    limit.str
}
    
## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.table"),
    def = function (x, nrows = 100, array = TRUE) {
        warn.r <- getOption("warn")
        options(warn = -1)
        if (array) {
            x <- .expand.array(x)
            res <- .db.getQuery(paste("select * from (", content(x), ") s",
                                      .limit.str(nrows), sep = ""),
                                conn.id(x))
        } else 
            res <- .db.getQuery(paste("select * from ", content(x),
                                      .limit.str(nrows), sep = ""),
                                conn.id(x))
        options(warn = warn.r) # reset R warning level
        res
    })

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.view"),
    def = function (x, nrows = 100, interactive = FALSE, array = TRUE) {
        warn.r <- getOption("warn")
        options(warn = -1)
        if (interactive) {
            cat(deparse(substitute(x)),
                "points to a view in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        if (array) {
            x <- .expand.array(x)
            res <- .db.getQuery(paste("select * from (", content(x), ") s",
                                      .limit.str(nrows), sep = ""),
                                conn.id(x))
        } else 
            res <- .db.getQuery(paste("select * from ", content(x),
                                      .limit.str(nrows), sep = ""),
                                conn.id(x))
        options(warn = warn.r) # reset R warning level
        res
    })

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rquery"),
    def = function (x, nrows = 100, interactive = FALSE, array = TRUE) {
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

        if (array) x <- .expand.array(x)
        res <- .db.getQuery(paste(content(x), .limit.str(nrows),
                                  sep = ""), conn.id(x))

        msg.level <- .set.msg.level(msg.level, conn.id(x)) # reset message level
        options(warn = warn.r) # reset R warning level

        if (length(names(x)) == 1 && x@.col.data_type == "array") {
            if (gsub("int", "", x@.col.udt_name) != x@.col.udt_name)
                res <- arraydb.to.arrayr(res[[1]], "integer")
            else if (gsub("float", "", x@.col.udt_name) != x@.col.udt_name)
                res <- arraydb.to.arrayr(res[[1]], "double")
            else if (x@.col.udt_name %in% c("_bool"))
                res <- arraydb.to.arrayr(res[[1]], "logical")
            else
                res <- arraydb.to.arrayr(res[[1]], "character")
            if (dim(res)[1] == 1)
                res <- as.vector(res)
        }
           
        return (res)
    })

## ------------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rcrossprod"),
    def = function (x, interactive = FALSE) {
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

        res <- .db.getQuery(content(x), conn.id(x))

        dims <- x@.dim

        res <- arraydb.to.arrayr(res[1,1], "double")
        res <- matrix(res, nrow = dims[1], ncol = dims[2])
        
        msg.level <- .set.msg.level(msg.level, conn.id(x)) # reset message level
        options(warn = warn.r) # reset R warning level
           
        return (res)
    })


## ------------------------------------------------------------------------

## same as preview
lookat <- function (x, nrows = 100, array = TRUE)
{
    
    if (is(x, "db.table")) return (preview(x, nrows, array = array))
    if (is(x, "db.Rcrossprod")) return (preview(x, FALSE))
    preview(x, nrows, FALSE, array)
}
