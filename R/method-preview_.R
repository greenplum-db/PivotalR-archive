## -----------------------------------------------------------------------
## Preview the object
## -----------------------------------------------------------------------

setGeneric (
    "preview",
    def = function (x, ..., drop=TRUE) {
        res <- standardGeneric("preview")
        if (!is.null(ncol(res)) && drop && ncol(res) == 1)
            return (res[, , drop=TRUE])
        res
    },
    signature = "x")

## -----------------------------------------------------------------------

.limit.str <- function (nrows)
{
    if (is.null(nrows) || (is.character(nrows) && nrows == "all") ||
        nrows <= 0)
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
    def = function (x, nrows = 100, array = TRUE, drop = TRUE) {
        if (length(names(x)) == 1 && x@.col.data_type == "array")
            z <- x[[names(x)]]
        else
            z <- x[,]
        lk(z, nrows=nrows, array=array, drop = drop)
    })

## -----------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.view"),
    def = function (x, nrows = 100, interactive = FALSE, array = TRUE,
    drop = TRUE) {
        ## warn.r <- getOption("warn")
        ## options(warn = -1)
        if (interactive) {
            cat(deparse(substitute(x)),
                "points to a view in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        ## if (array) {
        ##     x <- .expand.array(x)
        ##     res <- .db.getQuery(paste("select * from (", content(x), ") s",
        ##                               .limit.str(nrows), sep = ""),
        ##                         conn.id(x))
        ## } else
        ##     res <- .db.getQuery(paste("select * from ", content(x),
        ##                               .limit.str(nrows), sep = ""),
        ##                         conn.id(x))
        ## options(warn = warn.r) # reset R warning level
        ## res
        if (length(names(x)) == 1 && x@.col.data_type == "array")
            z <- x[[names(x)]]
        else
            z <- x[,]
        lk(z, nrows=nrows, array=array, drop = drop)
    })

## -----------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rquery"),
    def = function (x, nrows = 100, interactive = FALSE, array = TRUE,
    drop = TRUE) {
        warnings <- .suppress.warnings(conn.id(x))

        ## add.crossprod <- FALSE
        ## if (length(names(x)) > 1 && "crossprod" %in% x@.col.data_type) {
        ##     select <- which(x@.col.data_type == "crossprod")
        ##     rst <- list()
        ##     for (i in select) {
        ##         z <- x[,i]
        ##         class(z) <- "db.Rcrossprod"
        ##         rst[[names(x)[i]]] <- lk(z)
        ##     }
        ##     if (length(select) == length(names(x)))
        ##         return (rst)
        ##     else {
        ##         left <- setdiff(1:length(names(x)), select)
        ##         x <- x[left]
        ##         add.crossprod <- TRUE
        ##     }
        ## }

        if (interactive) {
            cat(deparse(substitute(x)),
                "is just a query in R and does not point to any object in the database",
                dbname(conn.id(x)),
                "and it might take time to evaluate and extract a preview of it if the data is large!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }

        ## if (array) x <- .expand.array(x)

        ## res <- .db.getQuery(paste(content(x), .limit.str(nrows),
        ##                           sep = ""), conn.id(x))
        res <- db.q(content(x), .limit.str(nrows), conn.id = conn.id(x),
                    verbose = FALSE, sep = "", nrows = -1)

        .restore.warnings(warnings)
        for (i in seq_len(length(names(x)))) {
            if (array && x@.col.data_type[i] == "array") {
                if (grepl("int", x@.col.udt_name[i]))
                    res[[i]] <- arraydb.to.arrayr(res[[i]], "integer")
                else if (grepl("float", x@.col.udt_name[i]))
                    res[[i]] <- arraydb.to.arrayr(res[[i]], "double")
                else if (x@.col.udt_name[i] == "_bool")
                    res[[i]] <- arraydb.to.arrayr(res[[i]], "logical")
                else
                    res[[i]] <- arraydb.to.arrayr(res[[i]], "character")
            }
        }
        if (dim(res)[1] == 1)
            res <- as.vector(res)

        ## if (add.crossprod) {
        ##     rst[[length(rst)+1]] <- res
        ##     return (rst)
        ## } else

        return (res)
    })

## -----------------------------------------------------------------------

setMethod (
    "preview",
    signature (x = "db.Rcrossprod"),
    def = function (x, nrows = 100, interactive = FALSE, drop = TRUE) {
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
        ## res <- .db.getQuery(paste("select unnest(\"", names(x)[1],
        ##                           "\") as v from (",
        ##                           content(x),
        ##                           .limit.str(nrows), ") s", sep = ""), conn.id(x))
        res <- db.q("select unnest(\"", names(x)[1],
                    "\") as v from (",
                    content(x),
                    .limit.str(nrows), ") s", sep = "",
                    conn.id = conn.id(x), verbose = FALSE, nrows = -1)

        n <- dim(x)[1]
        dims <- x@.dim

        if (n == 1) {
            ## rst <- arraydb.to.arrayr(res[,1], "double")
            if (x@.is.symmetric[1])
                rst <- new("dspMatrix", uplo = "U", x = res[,1], Dim = as.integer(dims))
            else
                rst <- new("dgeMatrix", x = res[,1], Dim = as.integer(dims))
            ## if (x@.inverse) rst <- solve(rst)
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
                ## if (x@.inverse) rst[[i]] <- solve(rst[[i]])
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
    def = function (x, conn.id = 1, nrows = 100, array = TRUE,
    drop = TRUE) {
        x <- db.data.frame(x, conn.id=conn.id, verbose = FALSE)
        lookat(x, nrows=nrows, array=array, drop=drop)
    })


## -----------------------------------------------------------------------

## same as preview
## lk is shorthand for lookat
lk <- lookat <- function (x, nrows = 100, array = TRUE, conn.id = 1, drop=TRUE)
{
    if (is(x, "db.table")) return (preview(x, nrows, array = array, drop=drop))
    if (is(x, "db.Rcrossprod")) return (preview(x, nrows, FALSE, drop=drop))
    if (is(x, "character")) return (preview(x, conn.id, nrows, array, drop=drop))
    preview(x, nrows, FALSE, array, drop=drop)
}
