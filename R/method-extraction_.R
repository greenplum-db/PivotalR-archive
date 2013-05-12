
## ------------------------------------------------------------------------
## extraction function $ and [[
## ------------------------------------------------------------------------

setMethod (
    "$",
    signature(x = "db.obj"),
    function (x, name) {
        if (! name %in% names(x)) {
            message(paste("Column", name, "does not exist!"))
            stop()
        }

        if (identical(x@.key, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.key, ", ", sep = "")

        if (is(x, "db.data.frame"))
            parent <- content(x) # as.character(match.call()$x)
        else
            parent <- x@.parent

        idx <- which(names(x) == name)
        new("db.Rquery",
            .content = paste("select ", id.str, name,
            " from ", content(x), sep = ""),
            .expr = name,
            .parent = parent,
            .conn.id = conn.id(x),
            .col.name = name,
            .key = x@.key,
            .col.data_type = x@.col.data_type[idx],
            .col.udt_name = x@.col.udt_name[idx])
            
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod(
    "[[",
    signature(x = "db.obj"),
    function (x, i, j, ...) {
        na <- nargs()
        if (na == 1) {
            message("Error : argument is missing!")
            stop()
        }

        if (identical(x@.key, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.key, ", ", sep = "")

        if (is(x, "db.data.frame"))
            parent <- content(x) # as.character(match.call()$x)
        else
            parent <- x@.parent
        
        if (na == 2)
        {
            if (length(i) > 1) {
                message("Error : subscript out of range!")
                stop()
            }
            if (is.character(i))
                if (i %in% names(x)) {
                    idx <- which(names(x) == i)
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i, " from ", content(x), sep = ""),
                        .expr = i,
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = i,
                        .key = x@.key,
                        .col.data_type = x@.col.data_type[idx],
                        .col.udt_name = x@.col.udt_name[idx])
                } else {
                    message(paste("Error : column", i, "does not exist!"))
                    stop()
                }
            else
            {
                ii <- as.integer(i)
                if (ii < 1 || ii > length(names(x))) {
                    message("Error : subscript out of range!")
                    stop()
                }

                new("db.Rquery",
                    .content = paste("select ", id.str,
                    names(x)[ii], " from ", content(x),
                    sep = ""),
                    .expr = names(x)[ii],
                    .parent = parent,
                    .conn.id = conn.id(x),
                    .col.name = names(x)[[ii]],
                    .key = x@.key,
                    .col.data_type = x@.col.data_type[ii],
                    .col.udt_name = x@.col.udt_name[ii])
            }
        }
        else if (na == 3)
        {
            
            if (identical(x@.key, character(0)) == 0) {
                message("Error : there is no unique ID associated",
                        " with each row of the table!")
                stop()
            }

            if (is.character(j))
                if (j %in% names(x)) {
                    col.name <- j
                    idx <- which(names(x) == j)
                } else {
                    stop(paste("Column", j, "does not exist!"))
                }
            else
            {
                jj <- as.integer(j)
                if (jj < 1 || jj > length(names(x))) {
                    message("Error : subscript out of range!")
                    stop()
                }
                col.name <- names(x)[jj]
                idx <- jj
            }

            new("db.Rquery",
                .content = paste("select ", id.str, col.name,
                " from ", content(x), " where ", x@.key,
                " = ", i, sep = ""),
                .expr = col.name,
                .parent = parent,
                .col.name = col.name,
                .conn.id = conn.id(x),
                .key = x@.key,
                .col.data_type = x@.col.data_type[idx],
                .col.udt_name = x@.col.udt_name[idx])
        }
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod (
    "[",
    signature (x = "db.obj"),
    function (x, i, j) {
        na <- nargs()
        if (na == 1) {
            message("Error : argument is missing!")
            stop()
        }

        if (identical(x@.key, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.key, ", ", sep = "")

        if (is(x, "db.data.frame"))
            parent <- content(x) # as.character(match.call()$x)
        else
            parent <- x@.parent
        
        if (na == 2) # select columns
        {
            if (is.character(i)) # allow vector
                if (all(i %in% names(x))) {
                    idx <- .gwhich(names(x) == i)
                    i.str <- paste(i, collapse = ",")
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i.str, " from ", content(x), sep = ""),
                        .expr = i,
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = i,
                        .key = x@.key,
                        .col.data_type = x@.col.data_type[idx],
                        .col.udt_name = x@.col.udt_name[idx])
                } else {
                    message(paste("Error : column", i, "does not exist!"))
                    stop()
                }
            else if (is(i, "logical")) # repeat
            {
                i.str <- paste(names(x)[i], collapse = ", ")
                new("db.Rquery",
                    .content = paste("select ", id.str,
                    i.str, " from ", content(x),
                    sep = ""),
                    .expr = names(x)[i],
                    .parent = parent,
                    .conn.id = conn.id(x),
                    .col.name = names(x)[i],
                    .key = x@.key,
                    .col.data_type = x@.col.data_type[i],
                    .col.udt_name = x@.col.udt_name[i])
            }
            else 
            {
                ii <- as.integer(i) # allow vector
                ii <- ii[ii != 0] # ignore zeros
                if ((all(ii > 0) &&
                     !all(ii %in% seq_len(length(names(x))))) ||
                    (all(ii < 0) &&
                     !all(-ii %in% seq_len(length(names(x))))))
                {
                    message("Error : subscript out of range!")
                    stop()
                }

                if (ii[1] < 0)
                    ii <- setdiff(seq_len(length(names(x))), -ii)
                i.str <- paste(names(x)[ii], collapse = ", ")
                new("db.Rquery",
                    .content = paste("select ", id.str,
                    i.str, " from ", content(x),
                    sep = ""),
                    .expr = names(x)[ii],
                    .parent = parent,
                    .conn.id = conn.id(x),
                    .col.name = names(x)[ii],
                    .key = x@.key,
                    .col.data_type = x@.col.data_type[ii],
                    .col.udt_name = x@.col.udt_name[ii])
            }
        }
        else if (na == 3) # several cases
        {
            if (missing(i))
            {
                if (missing(j)) {
                    ## A Rquery copy of the original object
                    return (new("db.Rquery",
                                .content = paste("select * from",
                                content(x)),
                                .expr = names(x),
                                .parent = parent,
                                .con.id = conn.id(x),
                                .col.name = names(x),
                                .key = x@.key))
                }

                if (is(j, "db.Rquery"))
                    j <- .db.getQuery(content(j), conn.id(x))
                
                if (is(j, "character"))
                {
                    if (all(j %in% names(x))) {
                        j.str <- paste(j, collapse = ", ")
                        new("db.Rquery",
                            .content = paste("select ", id.str, j.str,
                            " from ", content(x), sep = ""),
                            .expr = j,
                            .parent = parent,
                            .conn.id = conn.id(x),
                            .col.name = j,
                            .key = x@.key)
                    } else {
                        message(paste("Error : column", i,
                                      "does not exist!"))
                        stop()
                    }
                }
                else if (is(j, "logical"))
                {
                    j.str <- paste(names(x)[j], collapse = ", ")
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i.str, " from ", content(x),
                        sep = ""),
                        .expr = names(x)[j],
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = names(x)[j],
                        .key = x@.key,
                        .col.data_type = x@.col.data_type[j],
                        .col.udt_name = x@.col.udt_name[j])
                }
                else
                {
                    jj <- integer(j)
                    jj <- jj[jj != 0] # ignore zero
                    if ((all(jj > 0) &&
                         !all(jj %in% seq_len(length(names(x))))) ||
                        (all(jj < 0) &&
                         !all(-jj %in% seq_len(length(names(x))))))
                    {
                        message("Error : subscript out of range!")
                        stop()
                    }

                    if (jj[1] < 0)
                        jj <- setdiff(seq_len(length(names(x))), -jj)
                    j.str <- paste(names(x)[jj], collapse = ", ")
                    new("db.Rquery",
                        .content = paste("select ", id.str, j.str,
                        " from ", content(x), sep = ""),
                        .expr = names(x)[jj],
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = names(x)[jj],
                        .key = x@.key)
                }
            }
            else if (is(i, "db.Rquery") &&
                     (missing(j) || is(j, "db.Rquery")))
            {

            }
            else if (is(i, "db.Rquery") && !is.(j, "db.Rquery"))
            {

            }
            else if (!is(i, "db.Rquery") &&
                     (missing(j) || is(j, "db.Rquery")))
            {

            }
            else if (!is(i, "db.Rquery") && !is(j, "db.Rquery"))
            {
                if (identical(x@.key, character(0)) == 0) {
                    message("Error : there is no unique ID associated",
                            " with each row of the table!")
                    stop()
                }

                where.str <- paste(x@.key, "=", i, collapse = "or")

                if (is.character(j))
                    if (all(j %in% names(x))) {
                        col.name <- j
                    } else {
                        stop(paste("Column", j, "does not exist!"))
                    }
                else if (is(j, "logical"))
                {
                    j.str <- paste(names(x)[j], collapse = ", ")
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        j.str, " from ", content(x), " where ",
                        where.str, sep = ""),
                        .expr = names(x)[j],
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = names(x)[j],
                        .key = x@.key,
                        .col.data_type = x@.col.data_type[j],
                        .col.udt_name = x@.col.udt_name[j])
                }
                else
                {
                    jj <- as.integer(j)
                    if (! all(jj, seq_len(length(names(x))))) {
                        message("Error : subscript out of range!")
                        stop()
                    }
                    col.name <- names(x)[jj]
                }
                
                new("db.Rquery",
                    .content = paste("select ", id.str, col.name,
                    " from ", content(x), " where ", where.str, sep = ""),
                    .expr = col.name,
                    .parent = parent,
                    .col.name = col.name,
                    .conn.id = conn.id(x),
                    .key = x@.key)
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
