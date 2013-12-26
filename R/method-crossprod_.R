
## -----------------------------------------------------------------------
## compute the crossprod for row-saved X and Y, t(X) %*% Y
## Each row of X and Y is an array
## -----------------------------------------------------------------------

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
        is.symmetric <- eql(x, y) # a symmetric matrix

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
            ## a <- .get.array.elements(s, tbl, where.str, conn.id)
            a <- s
        } else { # if (all(x@.col.data_type != "array"))
            x <- db.array(x)
            a <- x@.expr # paste("(", s, ")", sep = "")
        } ## else
          ##   stop(deparse(substitute(x)), " is not a proper matrix!")
        m <- .col.number.all(x) # compute the column numbers including array
        b <- a
        n <- m # if it is symmetric
  
        if (!is.symmetric) {
            if (is(y, "db.data.frame")) s <- names(y)
            else s <- y@.expr
            if (length(s) == 1 && y@.col.data_type == "array") {
                ## b <- .get.array.elements(s, tbl, where.str, conn.id)
                b <- s
            } else { # if (all(y@.col.data_type != "array"))
                ## b <- paste("(", s, ")", sep = "")
                y <- db.array(y)
                b <- y@.expr
            } ## else
            ##     stop(deparse(substitute(y)), " is not a proper matrix!")
            ## n <- length(b)
            n <- .col.number.all(y)
        }

        ## tmp <- outer(a, b, function(x, y){paste0(x, " * ", y)})
        ## if (is.symmetric) tmp <- tmp[upper.tri(tmp, diag = TRUE)]
        ## expr <- paste0("sum(array[", paste0(tmp, collapse = ", "), "])")pg90 mad

        db.info <- .get.dbms.str(conn.id)
        madlib <- schema.madlib(conn.id)
        quick <- db.q("select count(*)  from pg_catalog.pg_proc p LEFT JOIN",
                      "pg_catalog.pg_namespace n ON n.oid = p.pronamespace",
                      "where p.proname ~ '^(crossprod)$' and",
                      "n.nspname ~ '^(madlib)$'", conn.id = conn.id, verbose = FALSE)
        if (quick == 0) {
            if (is.symmetric) { ## Using symmetric specific function is faster
                ## func <- .load.func("crossprod_double", conn.id)
                ## expr <- paste0("sum(", func, "(", a, "))")
                ## is.symmetric <- FALSE
                func <- .load.func("crossprod_sym_double", conn.id)
                expr <- paste("sum(", func, "(", a, "))", sep = "")
            } else {
                func <- .load.func("crossprod_double2", conn.id)
                expr <- paste("sum(", func, "(", a, ", ", b, "))", sep = "")
            }
            if (db.info$db.str == "PostgreSQL") {
                expr <- paste(madlib, ".__array_", expr, sep = "")
            }
        } else {
            if (is.symmetric)
                expr <- paste(madlib, ".crossprod_sym(", a, ")", sep = "")
            else
                expr <- paste(madlib, ".crossprod(", a, ", ",
                              b, ")", sep = "")
        }

        new("db.Rcrossprod",
            .content = paste("select ", expr, " as cross_prod from ",
            tbl, where.str, sort$str, sep = ""),
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id,
            .col.name = "cross_prod",
            .key = character(0),
            .where = where,
            .col.data_type = "crossprod",
            .col.udt_name = "_float",
            .is.factor = FALSE,
            .factor.ref = as.character(NA),
            .factor.suffix = "",
            .sort = sort,
            .is.crossprod = TRUE,
            .is.symmetric = is.symmetric,
            .dim = c(m,n),
            .dist.by = x@.dist.by)
    })

