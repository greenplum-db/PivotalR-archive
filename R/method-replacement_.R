
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
        idx <- which(x.names == name)
        x.names[idx] <- paste(value@.expr, " as ", name, sep = "")
        x.col.data_type[idx] <- value@.col.data_type
        x.col.udt_name[idx] <- value@.col.udt_name
        expr <- paste(x.names, collapse = ", ")

        if (value@.parent != value@.source) 
            tbl <- paste("(", value@.parent, ")", sep = "")
        else
            tbl <- value@.parent
        
        new("db.Rquery",
            .content = paste("select ", expr, " from ",
            tbl, " s", sep = ""),
            .expr = x.names,
            .source = value@.source,
            .parent = value@.parent,
            .conn.id = conn.id(x),
            .col.name = names(x),
            .key = x@.key,
            .col.data_type = x.col.data_type,
            .col.udt_name = x.col.udt_name)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

## Similar to the above function, but for [[
setMethod (
    "[[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
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
        if (is(i, "character")) {
            idx <- which(x.names == i)
            x.names[idx] <- paste(value@.expr, " as ", i, sep = "")
        } else if (is(i, "numeric")) {
            idx <- i
            x.names[idx] <- paste(value@.expr, " as ", x.names[idx], sep = "")
        }
        x.col.data_type[idx] <- value@.col.data_type
        x.col.udt_name[idx] <- value@.col.udt_name

        if (value@.parent != value@.source) 
            tbl <- paste("(", value@.parent, ")", sep = "")
        else
            tbl <- value@.parent
        
        expr <- paste(x.names, collapse = ", ")
        new("db.Rquery",
            .content = paste("select ", expr, " from ",
            tbl, " s", sep = ""),
            .expr = x.names,
            .source = value@.source,
            .parent = value@.parent,
            .conn.id = conn.id(x),
            .col.name = names(x),
            .key = x@.key,
            .col.data_type = x.col.data_type,
            .col.udt_name = x.col.udt_name)
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
