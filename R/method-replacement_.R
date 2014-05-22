
## -----------------------------------------------------------------------
## replacement methods
## -----------------------------------------------------------------------

.replacement <- function (x, name, value, case = NULL, left.where = NULL)
{
    if (!conn.eql(conn.id(x), conn.id(value)))
        stop("Both sides must be derived from objects in the same connection!")

    ## One cannot do this without any restrictions
    if ((is(x, "db.data.frame") && content(x) != value@.parent) ||
        (is(x, "db.Rquery") && x@.parent != value@.parent))
        stop(paste("This operation can only be done if both sides",
                   "are derived from the same database object!"))

    ## value cannot have multiple columns
    if (length(name) != length(value@.expr))
        stop(paste("The value on the right cannot be",
                   "assign to a column on the left."))

    if ((is.null(left.where) &&
        ((is(x, "db.data.frame") && value@.where != "") ||
        (is(x, "db.Rquery") && !.eql.where(.strip(x@.where), .strip(value@.where)))))
        || (!is.null(left.where) && !.eql.where(.strip(left.where), .strip(value@.where))))
        stop("The where parts that do not match!")

    if (is(x, "db.data.frame")) {
        x.expr <- paste("\"", names(x), "\"", sep = "")
        x.names <- x.expr
    } else {
        x.expr <- x@.expr
        x.names <- x@.expr
    }
    is.factor <- x@.is.factor
    factor.ref <- x@.factor.ref
    factor.suffix <- x@.factor.suffix
    x.col.data_type <- x@.col.data_type
    x.col.udt_name <- x@.col.udt_name
    x.col.name <- x@.col.name
    for (i in seq_len(length(name)))
    {
        idx <- which(x@.col.name == name[i])
        if (identical(idx, integer(0))) { # a new column
            if (is.null(case))
                x.names <- c(x.names, value@.expr[i])
            else
                stop("must create a column for all rows")
            x.col.name <- c(x.col.name, name[i])
            x.col.data_type <- c(x.col.data_type, value@.col.data_type[i])
            x.col.udt_name <- c(x.col.udt_name, value@.col.udt_name[i])
            is.factor <- c(is.factor, value@.is.factor[i])
            factor.ref <- c(factor.ref, value@.factor.ref[i])
            factor.suffix <- c(factor.suffix, value@.factor.suffix[i])
        } else {
            if (is.null(case))
                x.names[idx] <- value@.expr[i]
            else {
                if (is.na(value@.expr[i]))
                    vstr <- "NULL"
                else
                    vstr <- paste(value@.expr[i], "::", x@.col.data_type[idx],
                                  sep = "")
                x.names[idx] <- paste("case when ", case, " then ",
                                      vstr,
                                      " else ", x.expr[idx],
                                      " end", sep = "")
            }
            x.col.data_type[idx] <- value@.col.data_type[i]
            x.col.udt_name[idx] <- value@.col.udt_name[i]
            is.factor[idx] <- value@.is.factor[i]
            factor.ref[idx] <- value@.factor.ref[i]
            factor.suffix[idx] <- value@.factor.suffix[i]
        }
    }

    expr <- paste(x.names, paste("\"", x.col.name, "\"", sep = ""),
                  sep = " as ", collapse = ", ")

    if (value@.parent != value@.source)
        tbl <- paste("(", value@.parent, ") s", sep = "")
    else
        tbl <- value@.parent

    if (is(x, "db.Rquery") && x@.where != "") {
        where.str <- paste(" where", x@.where)
        where <- x@.where
    } else {
        where.str <- ""
        where <- ""
    }

    sort <- .generate.sort(x)

    new("db.Rquery",
        .content = paste("select ", expr, " from ",
        tbl, where.str, sort$str, sep = ""),
        .expr = x.names,
        .source = value@.source,
        .parent = value@.parent,
        .conn.id = conn.id(x),
        .col.name = x.col.name,
        .key = x@.key,
        .where = where,
        .col.data_type = x.col.data_type,
        .col.udt_name = x.col.udt_name,
        .is.factor = is.factor,
        .factor.ref = factor.ref,
        .factor.suffix = factor.suffix,
        .sort = sort,
        .dist.by = x@.dist.by)
}

## -----------------------------------------------------------------------

# replace a single value
.replace.single <- function (x, name, value, type, udt, case = NULL)
{
    if (is(x, "db.data.frame")) x <- x[,]
    x.names <- x@.expr
    is.factor <- x@.is.factor
    factor.ref <- x@.factor.ref
    factor.suffix <- x@.factor.suffix
    x.col.data_type <- x@.col.data_type
    x.col.udt_name <- x@.col.udt_name
    x.col.name <- x@.col.name
    if (length(case) == 1) case <- rep(case, length(name))
    for (i in seq_len(length(name)))
    {
        idx <- which(x@.col.name == name[i])
        if (identical(idx, integer(0))) { # a new column
            if (is.null(case))
                x.names <- c(x.names, as.character(value))
            else
                stop("must create a column for all rows")
            x.col.name <- c(x.col.name, name[i])
            x.col.data_type <- c(x.col.data_type, type)
            x.col.udt_name <- c(x.col.udt_name, udt)
            is.factor <- c(is.factor, FALSE)
            factor.ref <- c(factor.ref, as.character(NA))
            factor.suffix <- c(factor.suffix, "")
        } else {
            if (is.null(case))
                x.names[idx] <- as.character(value)
            else
                if (case[i] != "NULL") {
                    if (is.na(value))
                        vstr <- "NULL"
                    else
                        vstr <- paste(value, "::",
                                      x@.col.data_type[idx],
                                      sep = "")
                    x.names[idx] <- paste("case when ", case[i], " then ",
                                          vstr,
                                          " else ", x@.expr[idx],
                                          " end", sep = "")
                }
            x.col.data_type[idx] <- type
            x.col.udt_name[idx] <- udt
            is.factor[idx] <- FALSE
            factor.ref[idx] <- as.character(NA)
            factor.suffix[idx] <- ""
        }
    }

    expr <- paste(x.names, paste("\"", x.col.name, "\"", sep = ""),
                  sep = " as ", collapse = ", ")

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
        where.str <- paste(" where", x@.where)
        where <- x@.where
    } else {
        where.str <- ""
        where <- ""
    }

    sort <- .generate.sort(x)

    new("db.Rquery",
        .content = paste("select ", expr, " from ",
        tbl, where.str, sort$str, sep = ""),
        .expr = x.names,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(x),
        .col.name = x.col.name,
        .key = x@.key,
        .where = where,
        .col.data_type = x.col.data_type,
        .col.udt_name = x.col.udt_name,
        .is.factor = is.factor,
        .factor.ref = factor.ref,
        .factor.suffix = factor.suffix,
        .sort = sort,
        .dist.by = x@.dist.by)
}

## -----------------------------------------------------------------------

## when the value is db.Rquery, both x and value
## must be derived from a same ancestor.
## Otherwise, how can you match rows?
## If you want to do more complicated things,
setMethod (
    "$<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, name, value) {
        if (length(name) != 1) stop("Cannot replace multiple columns")
        if (is(value, "db.Rquery") && value@.is.agg) {
            value <- as.numeric(lookat(value))
            x[[name]] <- value
            return (x)
        }
        .replacement(x, name, value)
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "character"),
    function (x, name, value) {
        if (length(name) != 1) stop("Cannot replace multiple columns")
        .replace.single(x, name, paste("'", value, "'", sep = ""),
                        "text", "text")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "integer"),
    function (x, name, value) {
        if (length(name) != 1) stop("Cannot replace multiple columns")
        .replace.single(x, name, value, "integer", "int4")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "numeric"),
    function (x, name, value) {
        if (length(name) != 1) stop("Cannot replace multiple columns")
        .replace.single(x, name, value, "double precision", "float8")
    },
    valueClass = "db.Rquery")

setMethod (
    "$<-",
    signature (x = "db.obj", value = "logical"),
    function (x, name, value) {
        if (length(name) != 1) stop("Cannot replace multiple columns")
        .replace.single(x, name, value, "boolean", "bool")
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------

.preprocess.name <- function (x, i)
{
    if (is(i, "character"))
        name <- i
    else if (is(i, "numeric")) {
        idx <- i
        if (idx < 1 || idx > length(x@.col.name))
            stop("Subscript out of range!")
        name <- names(x)[idx]
    }
    name
}

## -----------------------------------------------------------------------

## Similar to the above function, but for [[
setMethod (
    "[[<-",
    signature (x = "db.obj", value = "db.Rquery"),
    function (x, i, j, value) {
        if (length(i) != 1) stop("Cannot replace multiple columns")
          if (is(value, "db.Rquery") && value@.is.agg) {
            value <- as.numeric(lookat(value))
            if (missing(i) && missing(j)) {
                x[[,]] <- value
                return (x)
            }
            if (missing(i)) {
                x[[,j]] <- value
                return (x)
            }
            if (missing(j)) {
                x[[i,]] <- value
                return (x)
            }
            x[[i,j]] <- value
            return (x)
        }
        name <- .preprocess.name(x, i)
        .replacement(x, name, value)
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "character"),
    function (x, i, j, value) {
        if (length(i) != 1) stop("Cannot replace multiple columns")
        name <- .preprocess.name(x, i)
        .replace.single(x, name, paste("'", value, "'", sep = ""),
                        "text", "text")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "integer"),
    function (x, i, j, value) {
        if (length(i) != 1) stop("Cannot replace multiple columns")
        name <- .preprocess.name(x, i)
        .replace.single(x, name, value, "integer", "int4")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "numeric"),
    function (x, i, j, value) {
        if (length(i) != 1) stop("Cannot replace multiple columns")
        name <- .preprocess.name(x, i)
        .replace.single(x, name, value, "double precision", "float8")
    },
    valueClass = "db.Rquery")

setMethod (
    "[[<-",
    signature (x = "db.obj", value = "logical"),
    function (x, i, j, value) {
        if (length(i) != 1) stop("Cannot replace multiple columns")
        name <- .preprocess.name(x, i)
        .replace.single(x, name, value, "boolean", "bool")
    },
    valueClass = "db.Rquery")

## -----------------------------------------------------------------------
## -----------------------------------------------------------------------

## the condition expression in case
.case.condition <- function (x, i)
{
    if (is(i, "db.Rquery")) {
        str <- i@.expr
        if (length(str) != 1)
            stop("More than 2 boolean expressions in selecting row!")
    } else if (!is(i, "db.Rquery")) {
        if (identical(x@.key, character(0))) {
            message("Error : there is no unique ID associated",
                    " with each row of the table!")
            stop()
        }

        ## where.str <- paste(x@.key, "=", i, collapse = " or ")
        str <- paste("\"", x@.key, "\" in (", paste(i, collapse = ","),
                     ")", sep = "")
    }
    str
}

## -----------------------------------------------------------------------

`[<-.db.obj` <- function(x, i, j, value)
{
    has.i <- !missing(i)
    has.j <- !missing(j)

    if (length(x@.col.name) == 1 && x@.col.data_type == "array" &&
        value@.is.factor)
        stop("Cannot set an array to be a factor!")

    if (is(value, "db.Rquery")) func <- "dbRquery"
    else if (is(value, "character")) func <- "character"
    else if (is(value, "integer")) func <- "integer"
    else if (is(value, "numeric")) func <- "numeric"
    else if (is(value, "logical")) func <- "logical"
    else stop("value type is not supported!")
    if (has.i && has.j)
        eval(parse(text = paste(".square.bracket.", func, "(x=x, i=i, j=j, value=value)", sep = "")))
    else if (has.i)
        eval(parse(text = paste(".square.bracket.", func, "(x=x, i=i, value=value)", sep = "")))
    else if (has.j)
        eval(parse(text = paste(".square.bracket.", func, "(x=x, j=j, value=value)", sep = "")))
    else
        eval(parse(text = paste(".square.bracket.", func, "(x=x, value=value)", sep = "")))
}

## ----------------------------------------------------------------------

.square.bracket.dbRquery <- function(x, i, j, value)
{
    n <- nargs()
    if (is(value, "db.Rquery") && value@.is.agg) {
        value <- as.numeric(lookat(value))
        if (missing(i) && missing(j)) {
            x[,] <- value
            return (x)
        }
        if (missing(i)) {
            x[,j] <- value
            return (x)
        }
        if (missing(j)) {
            x[i,] <- value
            return (x)
        }
        x[i,j] <- value
        return (x)
    }

    if (length(x@.col.name) == 1 && x@.col.data_type == "array") {
        x <- .expand.array(x)
        if (n == 3)
            x[i] <- value
        else
            if (missing(i) && missing(j)) x[,] <- value
            else if (missing(i)) x[,j] <- value
            else if (missing(j)) x[i,] <- value
            else x[i,j] <- value
        return (db.array(x))
    }

    if (n == 4) {
        if (missing(i) && missing(j))
            .replacement(x, names(x), value)
        else if (missing(i))
            .replacement(x, names(x[,j]), value)
        else {
            str <- .case.condition(x, i)
            if (is(x, "db.data.frame") || x@.where == "")
                where.str <- str
            else
                where.str <- paste("(", x@.where, ") and (", str, ")", sep = "")
            if (missing(j))
                .replacement(x, names(x), value, str, where.str)
            else
                .replacement(x, names(x[i,j]), value, str, where.str)
        }
    } else if (n == 3) {
        if (missing(j)) {
           if (is(i, "db.Rquery")) {
                str <- .case.condition(x, i)
                .replacement(x, names(x[i]), value, str)
           } else
                .replacement(x, names(x[i]), value)
        } else
            .replacement(x, names(x[j]), value)
    }
}

## -----------------------------------------------------------------------

.square.bracket.character <- function(x, i, j, value)
{
    value <- paste("'", value, "'", sep = "")
    n <- nargs()
    if (length(x@.col.name) == 1 && x@.col.data_type == "array") {
        x <- .expand.array(x)
        if (n == 3)
            x[i] <- value
        else
            if (missing(i) && missing(j)) x[i,j] <- value
            else if (missing(i)) x[,j] <- value
            else if (missing(j)) x[i,] <- value
            else x[i,j] <- value
        return (db.array(x))
    }

    if (n == 4) {
        if (missing(i) && missing(j))
            .replace.single(x, names(x), value, "text", "text")
        else if (missing(i))
            .replace.single(x, names(x[,j]), value, "text", "text")
        else {
            str <- .case.condition(x, i)
            if (missing(j))
                if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                    .replace.single(x, names(x), value, "integer", "int4",
                                    i@.expr)
                else
                    .replace.single(x, names(x), value, "text", "text", str)
            else
                .replace.single(x, names(x[i,j]), value,
                                "text", "text", str)
        }
    } else if (n == 3) {
        if (missing(j)) {
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "text", "text",
                                i@.expr)
            else
                .replace.single(x, names(x[i]), value, "text", "text",
                                i@.expr)
        } else
            .replace.single(x, names(x[j]), value, "text", "text")
    }
}

## -----------------------------------------------------------------------

.square.bracket.integer <- function(x, i, j, value)
{
    n <- nargs()
    if (length(x@.col.name) == 1 && x@.col.data_type == "array") {
        x <- .expand.array(x)
        if (n == 3)
            x[i] <- value
        else
            if (missing(i) && missing(j)) x[i,j] <- value
            else if (missing(i)) x[,j] <- value
            else if (missing(j)) x[i,] <- value
            else x[i,j] <- value
        return (db.array(x))
    }
    if (n == 4) {
        if (missing(i) && missing(j))
            .replace.single(x, names(x), value, "integer", "int4")
        else if (missing(i))
            .replace.single(x, names(x[,j]), value, "integer", "int4")
        else if (missing(j))
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "integer", "int4",
                                i@.expr)
            else
                .replace.single(x, names(x), value, "integer", "int4",
                                .case.condition(x, i))
        else
            .replace.single(x, names(x[i,j]), value, "integer",
                            "int4", .case.condition(x, i))
    } else if (n == 3) {
        if (missing(j)) {
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "integer", "int4",
                                i@.expr)
            else
                .replace.single(x, names(x[i]), value, "integer", "int4")
        } else
            .replace.single(x, names(x[j]), value, "integer", "int4")
    }
}

## -----------------------------------------------------------------------

.square.bracket.numeric <- function(x, i, j, value)
{
    n <- nargs()
    if (length(x@.col.name) == 1 && x@.col.data_type == "array") {
        x <- .expand.array(x)
        if (n == 3)
            x[i] <- value
        else
            if (missing(i) && missing(j)) x[i,j] <- value
            else if (missing(i)) x[,j] <- value
            else if (missing(j)) x[i,] <- value
            else x[i,j] <- value
        return (db.array(x))
    }
    if (n == 4) {
        if (missing(i) && missing(j)) {
            .replace.single(x, names(x), value, "double precision",
                            "float8")
        } else if (missing(i)) {
            .replace.single(x, names(x[,j]), value, "double precision",
                            "float8")
        } else if (missing(j)) {
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean")) {
                .replace.single(x, names(x), value, "double precision",
                                "float8", i@.expr)
            } else
                .replace.single(x, names(x), value, "double precision",
                                "float8", .case.condition(x, i))
        } else {
            .replace.single(x, names(x[i,j]), value, "double precision",
                            "float8", .case.condition(x, i))
        }
    } else if (n == 3) {
        if (missing(j)) {
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "double precision",
                                "float8", i@.expr)
            else
                .replace.single(x, names(x[i]), value, "double precision",
                                "float8")
        } else
            .replace.single(x, names(x[j]), value, "double precision",
                            "float8")
    }
}

## -----------------------------------------------------------------------

.square.bracket.logical <- function(x, i, j, value)
{
    n <- nargs()
    if (length(x@.col.name) == 1 && x@.col.data_type == "array") {
        x <- .expand.array(x)
        if (n == 3)
            x[i] <- value
        else
            if (missing(i) && missing(j)) x[i,j] <- value
            else if (missing(i)) x[,j] <- value
            else if (missing(j)) x[i,] <- value
            else x[i,j] <- value
        return (db.array(x))
    }
    if (n == 4) {
        if (missing(i) && missing(j))
            .replace.single(x, names(x), value, "boolean", "bool")
        else if (missing(i))
            .replace.single(x, names(x[,j]), value, "boolean", "bool")
        else if (missing(j))
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "boolean", "bool",
                                i@.expr)
            else
                .replace.single(x, names(x), value, "boolean", "bool",
                                .case.condition(x, i))
        else
            .replace.single(x, names(x[i,j]), value, "boolean", "bool",
                            .case.condition(x, i))
    } else if (n == 3) {
        if (missing(j)) {
            if (is(i, "db.Rquery") && all(i@.col.data_type == "boolean"))
                .replace.single(x, names(x), value, "boolean", "bool",
                                i@.expr)
            else
                .replace.single(x, names(x[i]), value, "boolean", "bool")
        } else
            .replace.single(x, names(x[i]), value, "boolean", "bool")
    }
}
