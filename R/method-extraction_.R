
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
        
        new("db.Rquery",
            .content = paste("select ", id.str, name,
            " from ", content(x), sep = ""),
            .expr = name,
            .parent = parent,
            .conn.id = conn.id(x),
            .col.name = name,
            .key = x@.key)
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
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i, " from ", content(x), sep = ""),
                        .expr = i,
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = i,
                        .key = x@.key)
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
                    .key = x@.key)
            }
        }
        else if (na == 3)
        {
            
            if (identical(x@.key, character(0)) == 0) {
                message("Error : there is no unique ID associated with each row of the table!")
                stop()
            }

            if (is.character(j))
                if (j %in% names(x)) {
                    col.name <- j
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
            }

            new("db.Rquery",
                .content = paste("select ", id.str, col.name,
                " from ", content(x), " where ", x@.key,
                " = ", i, sep = ""),
                .expr = col.name,
                .parent = parent,
                .col.name = col.name,
                .conn.id = conn.id(x),
                .key = x@.key)
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
                    i.str <- paste(i, collapse = ",")
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i.str, " from ", content(x), sep = ""),
                        .expr = i,
                        .parent = parent,
                        .conn.id = conn.id(x),
                        .col.name = i,
                        .key = x@.key)
                } else {
                    message(paste("Error : column", i, "does not exist!"))
                    stop()
                }
            else
            {
                ii <- as.integer(i) # allow vector
                if (! all(ii %in% seq_len(length(names(x))))) {
                    message("Error : subscript out of range!")
                    stop()
                }

                i.str <- paste(names(x)[ii], collapse = ", ")
                new("db.Rquery",
                    .content = paste("select ", id.str,
                    i.str, " from ", content(x),
                    sep = ""),
                    .expr = names(x)[ii],
                    .parent = parent,
                    .conn.id = conn.id(x),
                    .col.name = names(x)[ii],
                    .key = x@.key)
            }
        }
        else if (na == 3) # several cases
            ## case 1 : x[1,2]
            ## case 2 : x[1:2,3]
            ## case 3 : x[,3]
            ## case 4 : x[1:3,]
        {
            if (is(i, "db.Rquery") && is(j, "db.Rquery"))
            {

            }
            else if (is(i, "db.Rquery") && !is.(j, "db.Rquery"))
            {

            }
            else if (!is(i, "db.Rquery") && is(j, "db.Rquery"))
            {

            }
            else if (!is(i, "db.Rquery") && !is(j, "db.Rquery"))
            {
                if (identical(x@.key, character(0)) == 0) {
                    message("Error : there is no unique ID associated with each row of the table!")
                    stop()
                }

                if (is.character(j))
                    if (all(j %in% names(x))) {
                        col.name <- j
                    } else {
                        stop(paste("Column", j, "does not exist!"))
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
                    " from ", content(x), " where ", x@.key,
                    " = ", i, sep = ""),
                    .expr = col.name,
                    .parent = parent,
                    .col.name = col.name,
                    .conn.id = conn.id(x),
                    .key = x@.key)
            }
        }
    },
    valueClass = "db.Rquery")
