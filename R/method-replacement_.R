
## ------------------------------------------------------------------------
## replacement methods
## ------------------------------------------------------------------------

## when the value is db.Rquery, both x and value
## must be derived from a same ancestor.
## Otherwise, how can you match rows?
## If you want to do more complicated things,
setMethod (
    "$<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, name, value) {
        if (!conn.eql(conn.id(x), conn.id(value)))
            stop("Both sides must be derived from objects in the same connection!")
        
        ## One cannot do this without any restrictions
        if (all(content(x) != value@.parent))
            stop(paste("This operation can only be done if both sides",
                       "are derived from the same database object!"))

        ## value cannot have multiple columns
        if (length(names(value)) != 1 || length(value@.expr) != 1)
            stop(paste("The value on the right cannot be",
                       "assign to a column on the left."))

        ## They have to be in the same database
        if (!conn.eql(conn.id(x), conn.id(value)))
            stop(paste("The left side and right side are not related to",
                       "the same database!"))

        ## The primary key should be the same
        if (!identical(x@.key, value@.key))
            stop(paste("The primary key of the two sides are different!",
                       "Something must have been wrong!"))

        x.names <- names(x)
        x.col.data_type <- x@.col.data_type
        x.col.udt_name <- x@.col.udt_name
        is.factor <- x@.is.factor
        idx <- which(x.names == name)
        if (identical(idx, integer(0))) { # a new column
            x.names <- c(x.names, name)
            x.col.data_type <- c(x.col.data_type, value@.col.data_type)
            x.col.udt_name <- c(x.col.udt_name, value@.col.udt_name)
            is.factor <- c(is.factor, value@.is.factor)
        } else {
            x.names[idx] <- paste(value@.expr, " as ", name, sep = "")
            x.col.data_type[idx] <- value@.col.data_type
            x.col.udt_name[idx] <- value@.col.udt_name
            is.factor[idx] <- value@.is.factor
        }
            
        expr <- paste(x.names, collapse = ", ")

        if (value@.parent != value@.source) 
            tbl <- paste("(", value@.parent, ") s", sep = "")
        else
            tbl <- value@.parent

        if (x@.where != "")
            where.str <- paste("where", x@.where)
        else
            where.str <- ""

        new("db.Rquery",
            .content = paste("select ", expr, " from ",
            tbl, " ", where.str, sep = ""),
            .expr = x.names,
            .source = value@.source,
            .parent = value@.parent,
            .conn.id = conn.id(x),
            .col.name = names(x),
            .key = x@.key,
            .where = x@.where,
            .col.data_type = x.col.data_type,
            .col.udt_name = x.col.udt_name,
            .is.factor = is.factor)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

## Similar to the above function, but for [[
setMethod (
    "[[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
        if (!conn.eql(conn.id(x), conn.id(value)))
            stop("Both sides must be derived from objects in the same connection!")
        
        ## One cannot do this without any restrictions
        if (all(content(x) != value@.parent))
            stop(paste("This operation can only be done if both sides",
                       "are derived from the same database object!"))

        ## value cannot have multiple columns
        if (length(names(value)) != 1 || length(value@.expr) != 1)
            stop(paste("The value on the right cannot be",
                       "assign to a column on the left."))

        ## They have to be in the same database
        if (!conn.eql(conn.id(x), conn.id(value)))
            stop(paste("The left side and right side are not related to",
                       "the same database!"))

        ## The primary key should be the same
        if (!identical(x@.key, value@.key))
            stop(paste("The primary key of the two sides are different!",
                       "Something must have been wrong!"))

        x.names <- names(x)
        x.col.data_type <- x@.col.data_type
        x.col.udt_name <- x@.col.udt_name
        is.factor <- x@.is.factor
        if (is(i, "character"))
            idx <- which(x.names == i)
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
        }

        if (identical(idx, integer(0))) { # a new column
            x.names <- c(x.names, name)
            x.col.data_type <- c(x.col.data_type, value@.col.data_type)
            x.col.udt_name <- c(x.col.udt_name, value@.col.udt_name)
            is.factor <- c(is.factor, value@.is.factor)
        } else {
            x.names[idx] <- paste(value@.expr, " as ", x.names[idx], sep = "")
            x.col.data_type[idx] <- value@.col.data_type
            x.col.udt_name[idx] <- value@.col.udt_name
            is.factor[idx] <- value@.is.factor
        }

        if (value@.parent != value@.source) 
            tbl <- paste("(", value@.parent, ") s", sep = "")
        else
            tbl <- value@.parent
        
        expr <- paste(x.names, collapse = ", ")

        if (x@.where != "")
            where.str <- paste("where", x@.where)
        else
            where.str <- ""

        new("db.Rquery",
            .content = paste("select ", expr, " from ",
            tbl, " ", where.str, sep = ""),
            .expr = x.names,
            .source = value@.source,
            .parent = value@.parent,
            .conn.id = conn.id(x),
            .col.name = names(x),
            .key = x@.key,
            .where = x@.where,
            .col.data_type = x.col.data_type,
            .col.udt_name = x.col.udt_name,
            .is.factor = is.factor)
    },
    valueClass = "db.Rquery")

# ------------------------------------------------------------------------

setMethod (
    "[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
        stop("To be implemented")
    },
    valueClass = "db.Rquery")
