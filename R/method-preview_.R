
## -----------------------------------------------------------------------
## Preview the object
## -----------------------------------------------------------------------

setGeneric (
    "preview",
    def = function (x, ...) standardGeneric("preview"),
    signature = "x")

## -----------------------------------------------------------------------

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
    
## -----------------------------------------------------------------------

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

## -----------------------------------------------------------------------

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

## -----------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rquery"),
    def = function (x, nrows = 100, interactive = FALSE, array = TRUE) {
        warnings <- .suppress.warnings(conn.id(x))

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

        .restore.warnings(warnings)

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

## -----------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rcrossprod"),
    def = function (x, nrows = 100, interactive = FALSE) {
        warnings <- .suppress.warnings(conn.id(x))

        if (interactive) {
            cat(deparse(substitute(x)),
                "is just a query in R and does not point to any object in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        ## NOTE: Unfortunately, RPostgreSQL cannot extract an array with elements
        ## more than 1500. So we have to use unnest to help to load a large array
        ## TODO: Create a separate array loading function to specifically deal
        ## with such situations and can be called by other functions.
        res <- .db.getQuery(paste("select unnest(", names(x)[1], ") as v from (",
                                  content(x),
                                  .limit.str(nrows), ") s", sep = ""), conn.id(x))

        n <- dim(x)[1]
        dims <- x@.dim
        
        if (n == 1) {
            ## rst <- arraydb.to.arrayr(res[,1], "double")
            if (x@.is.symmetric[1])
                rst <- new("dspMatrix", uplo = "U", x = res[,1], Dim = as.integer(dims))
            else
                rst <- new("dgeMatrix", x = res[,1], Dim = as.integer(dims))
        } else {
            rst <- list()
            l <- dim(res)[1] / n
            for (i in seq_len(n)) {
                ## rst[[i]] <- arraydb.to.arrayr(res[i,1], "double")
                if (x@.is.symmetric[i])
                    rst[[i]] <- new("dtpMatrix", uplo = "U",
                                    x = res[(i-1)*l + seq(l),1],
                                    Dim = as.integer(dims))
                else
                    rst[[i]] <- new("dgeMatrix", x = res[(i-1)*l + seq(l),1],
                                    Dim = as.integer(dims))
            }
        }

        .restore.warnings(warnings)
           
        return (rst)
    })

## ----------------------------------------------------------------------

## Directly read a table without wrapping it with db.table
setMethod (
    "preview",
    signature (x = "character"),
    def = function (x, conn.id = 1, nrows = 100, array = TRUE) {
        x <- db.data.frame(x, conn.id=conn.id, verbose = FALSE)
        lookat(x, nrows=nrows, array=array)
    })


## -----------------------------------------------------------------------

## same as preview
lookat <- function (x, nrows = 100, array = TRUE, conn.id = 1)
{
    if (is(x, "db.table")) return (preview(x, nrows, array = array))
    if (is(x, "db.Rcrossprod")) return (preview(x, nrows, FALSE))
    if (is(x, "character")) return (preview(x, conn.id, nrows, array))
    preview(x, nrows, FALSE, array)
}
