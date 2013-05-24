
## ------------------------------------------------------------------------
## replacement methods
## ------------------------------------------------------------------------

.replacement <- function (x, name, value)
{
    if (!conn.eql(conn.id(x), conn.id(value)))
        stop("Both sides must be derived from objects in the same connection!")
        
    ## One cannot do this without any restrictions
    if ((is(x, "db.data.frame") && content(x) != value@.parent) ||
        (is(x, "db.Rquery") && x@.parent != value@.parent))
        stop(paste("This operation can only be done if both sides",
                   "are derived from the same database object!"))
    
    ## value cannot have multiple columns
    if (length(names(value)) != 1 || length(value@.expr) != 1)
        stop(paste("The value on the right cannot be",
                   "assign to a column on the left."))
    
    ## The primary key should be the same
    if (!identical(x@.key, value@.key))
        stop(paste("The primary key of the two sides are different!",
                   "Something must have been wrong!"))

    if ((is(x, "db.data.frame") && value@.where != "") ||
        (is(x, "db.Rquery") && x@.where != value@.where))
        stop("The where parts do not match!")
    
    x.names <- x@.col.name
    is.factor <- x@.is.factor
    x.col.data_type <- x@.col.data_type
    x.col.udt_name <- x@.col.udt_name
    x.col.name <- x@.col.name
    idx <- which(x@.col.name == name)
    if (identical(idx, integer(0))) { # a new column
        x.names <- c(x.names, value@.expr)
        x.col.name <- c(x.col.name, name)
        x.col.data_type <- c(x.col.data_type, value@.col.data_type)
        x.col.udt_name <- c(x.col.udt_name, value@.col.udt_name)
        is.factor <- c(is.factor, value@.is.factor)
    } else {
        x.names[idx] <- value@.expr
        x.col.data_type[idx] <- value@.col.data_type
        x.col.udt_name[idx] <- value@.col.udt_name
        is.factor[idx] <- value@.is.factor
    }
    
    expr <- paste(x.names, x.col.name, sep = " as ", collapse = ", ")
    
    if (value@.parent != value@.source) 
        tbl <- paste("(", value@.parent, ") s", sep = "")
    else
        tbl <- value@.parent
    
    if (is(x, "db.Rquery") && x@.where != "") {
        where.str <- paste("where", x@.where)
        where <- x@.where
    } else {
        where.str <- ""
        where <- ""
    }
    
    new("db.Rquery",
        .content = paste("select ", expr, " from ",
        tbl, " ", where.str, sep = ""),
        .expr = x.names,
        .source = value@.source,
        .parent = value@.parent,
        .conn.id = conn.id(x),
        .col.name = x.col.name,
        .key = x@.key,
        .where = where,
        .col.data_type = x.col.data_type,
        .col.udt_name = x.col.udt_name,
        .is.factor = is.factor)
}

## ------------------------------------------------------------------------

# replace a single value
.replace.single <- function (x, name, value, type, udt)
{
    x.names <- x@.col.name
    is.factor <- x@.is.factor
    x.col.data_type <- x@.col.data_type
    x.col.udt_name <- x@.col.udt_name
    x.col.name <- x@.col.name
    idx <- which(x@.col.name == name)
    if (identical(idx, integer(0))) { # a new column
        x.names <- c(x.names, value)
        x.col.name <- c(x.col.name, name)
        x.col.data_type <- c(x.col.data_type, type)
        x.col.udt_name <- c(x.col.udt_name, udt)
        is.factor <- c(is.factor, FALSE)
    } else {
        x.names[idx] <- value
        x.col.data_type[idx] <- type
        x.col.udt_name[idx] <- udt
        is.factor[idx] <- FALSE
    }
    
    expr <- paste(x.names, x.col.name, sep = " as ", collapse = ", ")

    if (is(x, "db.data.frame")) {
        tbl <- content(x)
        parent <- content(x)
        src <- parent
    } else {
        if (x@.parent != x@.source) {
            tbl <- paste("(", x@.parent, ") s", sep = "")
        } else {
            tbl <- x@.parent
        }
        parent <- x@.parent
        src <- x@.source
    }
    
    if (is(x, "db.Rquery") && x@.where != "") {
        where.str <- paste("where", x@.where)
        where <- x@.where
    } else {
        where.str <- ""
        where <- ""
    }
    
    new("db.Rquery",
        .content = paste("select ", expr, " from ",
        tbl, " ", where.str, sep = ""),
        .expr = x.names,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = x.col.name,
        .key = x@.key,
        .where = where,
        .col.data_type = x.col.data_type,
        .col.udt_name = x.col.udt_name,
        .is.factor = is.factor)
}

## ------------------------------------------------------------------------

## when the value is db.Rquery, both x and value
## must be derived from a same ancestor.
## Otherwise, how can you match rows?
## If you want to do more complicated things,
setMethod (
    "$<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, name, value) {
        .replacement(x, name, value)
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "character"),
    function (x, name, value) {
        .replace.single(x, name, value, "text", "text")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "integer"),
    function (x, name, value) {
        .replace.single(x, name, value, "integer", "int4")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "numeric"),
    function (x, name, value) {
        .replace.single(x, name, value, "double precision", "float8")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "logical"),
    function (x, name, value) {
        .replace.single(x, name, value, "boolean", "bool")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

## Similar to the above function, but for [[
setMethod (
    "[[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
        if (is(i, "character"))
            name <- i
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
            name <- names(x)[idx]
        }
        
        .replacement(x, name, value)
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "character"),
    function (x, i, j, value) {
        if (is(i, "character"))
            name <- i
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
            name <- names(x)[idx]
        }
        
        .replace.single(x, name, value, "text", "text")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "integer"),
    function (x, i, j, value) {
        if (is(i, "character"))
            name <- i
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
            name <- names(x)[idx]
        }
        
        .replace.single(x, name, value, "integer", "int4")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "numeric"),
    function (x, i, j, value) {
        if (is(i, "character"))
            name <- i
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
            name <- names(x)[idx]
        }
        
        .replace.single(x, name, value, "double precision", "float8")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "logical"),
    function (x, i, j, value) {
        if (is(i, "character"))
            name <- i
        else if (is(i, "numeric")) {
            idx <- i
            if (idx < 1 || idx > length(x@.col.name))
                stop("Subscript out of range!")
            name <- names(x)[idx]
        }
        
        .replace.single(x, name, value, "boolean", "bool")
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

setMethod (
    "[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
        stop("To be implemented")
    },
    valueClass = "db.Rquery")
