
## ------------------------------------------------------------------------
## extraction function $ and [[
## ------------------------------------------------------------------------

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

## ------------------------------------------------------------------------

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
                message("Error : there is no unique ID associated",
                        " with each row of the table!")
                stop()
            }
            if (x.where != "") x.where <- paste(x.where, "and")
            where.str <- paste(x@.key, "=", i)
            .create.db.Rquery(x, cols.i = j,
                              where = paste(x.where, where.str))
        }
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

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
        
        if (missing(i))
            i.missing <- TRUE
        else
            i.missing <- FALSE
        
        if (n == 3) {
            if (!i.missing && identical(x@.key, character(0)) &&
                !is(i, "db.Rquery")) {
                message("Error : there is no unique ID associated",
                        " with each row of the table!")
                stop()
            }
            if (!i.missing && is(i, "db.Rquery")) {
                if (length(i@.expr) > 1 || i@.col.data_type != "boolean") {
                    message("Error : cannot select columns!")
                    stop()
                }
            }
            if (missing(j))
                j <- seq_len(length(names(x)))
            else if (is(j, "db.Rquery"))
                j <- .db.getQuery(content(j), conn.id(x))
        }
        
        if (n == 2) { # select columns
            .create.db.Rquery(x, cols.i = i, x.where)
        } else if (n == 3) { # several cases
            if (i.missing) {
                .create.db.Rquery(x, cols.i = j, x.where)
            } else if (is(i, "db.Rquery")) {          
                where.str <- i@.expr
                if (length(where.str) != 1)
                    stop("More than 2 boolean expressions in selecting row!")
                if (x.where != "") x.where <- paste(x.where, "and")
                .create.db.Rquery(x, cols.i = j, where = paste(x.where, where.str))
            } else if (!is(i, "db.Rquery")) {
                if (identical(x@.key, character(0))) {
                    message("Error : there is no unique ID associated",
                            " with each row of the table!")
                    stop()
                }

                ## where.str <- paste(x@.key, "=", i, collapse = " or ")
                where.str <- paste(x@.key, " in (", paste(i, collapse = ","),
                                   ")", sep = "")
                if (x.where != "") x.where <- paste(x.where, "and")
                .create.db.Rquery(x, cols.i = j, where = paste(x.where, where.str))
            }
        }
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

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

## ------------------------------------------------------------------------

## Create db.Rquery in methods
.create.db.Rquery <- function (x, cols.i = NULL, where = "")
{
    if (is.null(cols.i) || !is.vector(cols.i)) {
        message("Error : column missing or format wrong!")
        stop()
    }

    if (is.character(cols.i)) {
        if (all(cols.i %in% names(x)))
            cols.i <- .gwhich(names(x), cols.i)
        else {
            message("Error : column does not exist!")
            stop()
        }            
    } else if (is(cols.i, "numeric")) {
        cols.i <- cols.i[cols.i != 0]
        if (length(cols.i) == 0) {
            message("Error : no column is selected!")
            stop()
        }
        idxs <- seq_len(length(names(x)))
        if ((all(cols.i > 0) && !all(cols.i %in% idxs)) ||
            (all(cols.i < 0) && !all(-cols.i %in% idxs))) {
            message("Error : subscript out of range!")
            stop()
        }

        if (cols.i[1] < 0)
            cols.i <- setdiff(idxs, -cols.i)
    }

    if (is(x, "db.data.frame"))
        expr <- x@.col.name[cols.i]
    else
        expr <- x@.expr[cols.i]
    i.str <- paste(expr, names(x)[cols.i], sep = " as ", collapse = ", ")

    if (is(x, "db.data.frame"))
        src <- content(x)
    else
        src <- x@.source

    if (is(x, "db.Rquery")) {
        tbl <- x@.parent
        parent <- x@.parent
    } else {
        tbl <- content(x)
        parent <- content(x)
    }

    is.factor <- x@.is.factor[cols.i]
    
    if (where != "") where.str <- paste("where", where)
    else where.str <- ""
    
    new("db.Rquery",
        .content = paste("select", i.str, "from", tbl, where.str),
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = names(x)[cols.i],
        .key = x@.key,
        .col.data_type = x@.col.data_type[cols.i],
        .col.udt_name = x@.col.udt_name[cols.i],
        .where = where,
        .is.factor = is.factor)
}

## ------------------------------------------------------------------------

setGeneric ("subset")

setMethod ("subset",
    signature(x = "db.obj"),
    function (x, subset, select)
    {
        x[subset, select]
    }
)
