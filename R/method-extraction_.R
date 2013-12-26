
## -----------------------------------------------------------------------
## extraction function $ and [[
## -----------------------------------------------------------------------

setMethod (
    "$",
    signature(x = "db.obj"),
    function (x, name) {
        if (is(x, "db.Rquery"))
            x.where <- x@.where
        else
            x.where <- ""
        .create.db.Rquery(x, cols.i = as.character(name), x.where)            
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------

setMethod(
    "[[",
    signature(x = "db.obj"),
    function (x, i, j, ...) {
        na <- nargs()
        if (is(x, "db.Rquery"))
            x.where <- x@.where
        else
            x.where <- ""
        if (na == 1) {
            message("Error : argument is missing!")
            stop()
        }

        if (na == 2) {
            if (length(i) > 1) {
                message("Error : subscript out of range!")
                stop()
            }

            if (is.logical(i)) i <- as.integer(i)
            .create.db.Rquery(x, cols.i = i, x.where)
        } else if (na == 3) {
            if (identical(x@.key, character(0))) {
                if (identical(i, logical(0))) {
                    return (structure(vector("list", length(names(x))),
                                      names = names(x), class = "data.frame"))
                } else {
                    message("Error : there is no unique ID associated",
                            " with each row of the table!")
                    stop()
                }
            }
            if (x.where != "") {
                x.where <- paste("(", x.where, ") and ", sep = "")
                lb <- "("
                rb <- ")"
            } else {
                lb <- ""
                rb <- ""
            }
            where.str <- paste(lb, x@.key, " = ", i, rb, sep = "")
            .create.db.Rquery(x, cols.i = j,
                              where = paste(x.where, where.str))
        }
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------

setMethod (
    "[",
    signature (x = "db.obj"),
    function (x, i, j, ...) {
        n <- nargs()
        if (is(x, "db.Rquery"))
            x.where <- x@.where
        else
            x.where <- ""
        if (n == 1) {
            message("Error : argument is missing!")
            stop()
        }

        ## if (length(names(x)) == 1) {
        ##     n <- 3
        ##     j <- 1
        ## }
        
        if (missing(i))
            i.missing <- TRUE
        else
            i.missing <- FALSE

        if (n == 3) {
            if (!i.missing && identical(x@.key, character(0)) &&
                !is(i, "db.Rquery")) {
                if (identical(i, logical(0)))
                    return (structure(vector("list", length(names(x))),
                                      names = names(x), class = "data.frame"))
                else {
                    message("Error : there is no unique ID associated",
                            " with each row of the table!")
                    stop()
                }
            }
            if (!i.missing && is(i, "db.Rquery")) {
                if (length(i@.expr) > 1 || i@.col.data_type != "boolean") {
                    message("Error : cannot select columns!")
                    stop()
                }
            }
            if (missing(j)) {
                if (length(names(x)) == 1 && x@.col.data_type == "array")
                    j <- NULL
                else
                    j <- seq_len(length(names(x)))
            } else if (is(j, "db.Rquery"))
                j <- .db.getQuery(paste(content(j), "limit 1"), conn.id(x))
        }

        if (n == 2 && length(names(x)) == 1 && is(i, "db.Rquery")) {
            n <- 3
            j <- 1
        }
        
        if (n == 2) { # select columns
            ## i.txt <- deparse(substitute(i))
            ## if (gsub("is.na\\s*\\(\\s*(.*\\S)\\s*\\)", "\\1", i.txt) ==
            ##     deparse(substitute(x))) { # i is is.na(...)
            ##     return (is.na(x))
            ## }
            if (is(i, "db.Rquery")) {
                if (all(i@.col.data_type == "boolean")) {
                    return (x[i,])
                } else {
                    i <- as.vector(unlist(.db.getQuery(paste(content(i), "limit 1"), conn.id(x))))
                    if (length(i) > length(names(x)))
                        stop("Are you sure the column number is correct?")
                }
            }
            .create.db.Rquery(x, cols.i = i, x.where)
        } else if (n == 3) { # several cases
            if (i.missing) {
                .create.db.Rquery(x, cols.i = j, x.where)
            } else if (is(i, "db.Rquery")) {          
                where.str <- i@.expr
                if (length(where.str) != 1)
                    stop("More than 2 boolean expressions in selecting row!")
                if (x.where != "") {
                    x.where <- paste("(", x.where, ") and ", sep = "")
                    lb <- "("
                    rb <- ")"
                } else {
                    lb <- ""
                    rb <- ""
                }
                .create.db.Rquery(x, cols.i = j, where = paste(x.where, lb,
                                                 where.str, rb, sep = ""))
            } else if (!is(i, "db.Rquery")) {
                if (identical(x@.key, character(0))) {
                    if (identical(i, logical(0))) {
                        return (structure(vector("list", length(names(x))),
                                          names = names(x),
                                          class = "data.frame"))
                    } else {
                        message("Error : there is no unique ID associated",
                                " with each row of the table!")
                        stop()
                    }
                }

                ## where.str <- paste(x@.key, "=", i, collapse = " or ")
                where.str <- paste("\"", x@.key, "\" in (", paste(i, collapse = ","),
                                   ")", sep = "")
                if (x.where != "") {
                    x.where <- paste(x.where, "and ")
                    lb <- "("
                    rb <- ")"
                } else {
                    lb <- ""
                    rb <- ""
                }
                .create.db.Rquery(x, cols.i = j, where = paste(x.where, lb, where.str,
                                                 rb, sep = ""))
            }
        }
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------

## utility function
## Find the indices of an array inside another array
## by repeatedly using which on each element.
## x is an array, and value is also an array
.gwhich <- function (x, value)
{
    res <- rep(0, length(value))
    for (i in seq_len(length(value)))
    {
        res[i] <- which(x == value[i])
    }
    res
}

## -----------------------------------------------------------------------

## Create db.Rquery in methods
.create.db.Rquery <- function (x, cols.i, where = "")
{
    if (!is.null(cols.i) && !is.vector(cols.i)) {
        message("Error : format wrong!")
        stop()
    }
    
    sort <- .generate.sort(x)
    
    if (is(x, "db.Rquery")) {
        if (x@.parent == x@.source)
            tbl <- x@.parent
        else
            tbl <- paste("(", x@.parent, ") s", sep = "")
        parent <- x@.parent
        src <- x@.source
    } else {
        tbl <- content(x)
        parent <- content(x)
        src <- content(x)
    }

    if (where != "") where.str <- paste(" where", where)
    else where.str <- ""

    is.col.i.char <- is.character(cols.i)
    
    if (is.col.i.char) {
        if (all(cols.i %in% names(x))) {
            cols.i <- .gwhich(names(x), cols.i)
            nss <- names(x)
        } else {
            return (NULL)
            ## message("Error : column does not exist!")
            ## stop()
        }            
    } else if (is(cols.i, "numeric") || is.null(cols.i)) {
        if (!is.null(cols.i)) {
            cols.i <- cols.i[cols.i != 0]
            if (length(cols.i) == 0) {
                message("Error : no column is selected!")
                stop()
            }
        }

        if (length(names(x)) == 1 && x@.col.data_type == "array") {
            if (is(x, "db.data.frame")) ep <- x@.col.name
            else ep <- x@.expr
            nss <- .get.array.elements(ep, tbl, where.str, conn.id(x))
        } else
            nss <- names(x)
        idxs <- seq_len(length(nss))
        if (!is.null(cols.i)) {
            if ((all(cols.i > 0) && !all(cols.i %in% idxs)) ||
                (all(cols.i < 0) && !all(-cols.i %in% idxs))) {
                message("Error : subscript out of range!")
                stop()
            }

            if (cols.i[1] < 0) cols.i <- setdiff(idxs, -cols.i)
        }
    }

    if (length(names(x)) == 1 && x@.col.data_type == "array"
        && !is.col.i.char) {
        if (is.null(cols.i)) cols.i <- seq_len(length(nss))
        expr <- nss[cols.i]
        col.name <- paste(x@.col.name, "[", cols.i, "]", sep = "")
    } else {
        if (is(x, "db.data.frame"))
            expr <- paste("\"", x@.col.name[cols.i], "\"", sep = "")
        else
            expr <- x@.expr[cols.i]
        col.name <- names(x)[cols.i]
    }
    
    if (length(expr) == 0) return (new("db.Rquery"))
    i.str <- paste(expr, paste("\"", col.name, "\"", sep = ""),
                   sep = " as ", collapse = ", ")

    col.data_type <- x@.col.data_type[cols.i]
    col.udt_name <- x@.col.udt_name[cols.i]
    is.factor <- x@.is.factor[cols.i]
    factor.ref <- x@.factor.ref[cols.i]
    factor.suffix <- x@.factor.suffix[cols.i]
    if (length(names(x)) == 1 && x@.col.data_type == "array"
        && !is.col.i.char && gsub("array_agg\\(.*\\)", "", expr) == expr) {
        idx <- which(.array.udt == x@.col.udt_name)
        col.data_type <- rep(.array.dat[idx], length(cols.i))
        col.udt_name <- rep(gsub("_", "", .array.udt[idx]), length(cols.i))
        is.factor <- rep(x@.is.factor, length(cols.i))
        factor.ref <- rep(x@.factor.ref, length(cols.i))
        factor.suffix <- rep(x@.factor.suffix, length(cols.i))
    }

    new("db.Rquery",
        .content = paste("select ", i.str, " from ", tbl, where.str,
        sort$str, sep = ""),
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = col.name,
        .key = x@.key,
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = where,
        .is.factor = is.factor,
        .factor.ref = factor.ref,
        .factor.suffix = factor.suffix,
        .sort = sort,
        .dist.by = x@.dist.by)
}

## -----------------------------------------------------------------------

setGeneric ("subset")

setMethod ("subset",
    signature(x = "db.obj"),
    function (x, subset, select) {
        n <- nargs()
        if (n == 1) return (x[,])
        if (n == 2) return (x[subset])
        if (n == 3) {
            if (missing(subset) && missing(select)) return (x[,])
            if (missing(subset)) return (x[,select])
            if (missing(select)) return (x[subset,])
        }
    })
