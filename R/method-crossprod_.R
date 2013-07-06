
## ------------------------------------------------------------------------
## compute the crossprod for row-saved X and Y, t(X) %*% Y
## Each row of X and Y is an array
## ------------------------------------------------------------------------

setGeneric ("crossprod")

setMethod (
    "crossprod",
    signature(x = "db.obj"),
    function (x, y = x) {
        if (is.null(y)) y <- x
        if (is(x, "db.Rcrossprod") || is(y, "db.Rcrossprod"))
            stop("Right now, db.Rcrossprod object is not supported!")
        if (!is(y, "db.obj"))
            stop("y must also be a db.obj just like x!")
        if (! conn.eql(conn.id(x), conn.id(y)))
            stop("x and y are not on the same database!")
        if (!.eql.parent(x, y))
            stop("x and y cannot match because they originate",
                 " from different sources!")
        conn.id <- conn.id(x)

        if (is(x, "db.data.frame")) {
            tbl <- content(x)
            src <- tbl
            parent <- tbl
        } else {
            if (x@.parent != x@.source)
                tbl <- paste("(", x@.parent, ") s", sep = "")
            else
                tbl <- x@.parent
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

        if (is(x, "db.data.frame")) s <- names(x)
        else s <- x@.expr
        if (length(s) == 1 && x@.col.data_type == "array") {
            a <- .get.array.elements(s, tbl, where.str, conn.id)
        } else if (all(x@.col.data_type != "array"))
            a <- paste("(", s, ")", sep = "")
        else
            stop(deparse(substitute(x)), " is not a proper matrix!")
        m <- length(a)

        if (is(y, "db.data.frame")) s <- names(y)
        else s <- y@.expr
        if (length(s) == 1 && y@.col.data_type == "array") {
            b <- .get.array.elements(s, tbl, where.str, conn.id)
        } else if (all(y@.col.data_type != "array"))
            b <- paste("(", s, ")", sep = "")
        else
            stop(deparse(substitute(y)), " is not a proper matrix!")
        n <- length(b)
 
        expr <- "array["
        for (j in seq_len(n)) {
            for (i in seq_len(m)) {
                expr <- paste(expr, "sum(", a[i], " * ", b[j], ")",
                              sep = "")
                if (i != m) expr <- paste0(expr, ", ")
            }
            if (j != n) expr <- paste0(expr, ", ")
        }
        expr <- paste0(expr, "]")

        new("db.Rcrossprod",
            .content = paste0("select ", expr, " as cross_prod from ",
            tbl, where.str, sort$str, sep = ""),
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id,
            .col.name = "cross_prod",
            .key = character(0),
            .where = where,
            .col.data_type = "array",
            .col.udt_name = "_float",
            .is.factor = FALSE,
            .factor.suffix = "",
            .sort = sort,
            .dim = c(m,n))
    })

