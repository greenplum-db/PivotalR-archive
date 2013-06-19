
## ------------------------------------------------------------------------
## compute the crossprod for row-saved X and Y, t(X) %*% Y
## Each row of X and Y is an array
## ------------------------------------------------------------------------

setGeneric ("crossprod")

setMethod (
    "crossprod",
    signature(x = "db.obj"),
    function (x, y = NULL) {
        if (is.null(y)) y <- x
        if (!is(y, "db.obj"))
            stop("y must also be a db.obj just like x!")
        if (! conn.eql(conn.id(x), conn.id(y)))
            stop("x and y are not on the same database!")
        conn.id <- conn.id(x)
        
        if (is(x, "db.data.frame"))
            tblx <- paste0(content(x), " s1")
        else
            tblx <- paste0("(", content(x), ") s1")
        if (is(y, "db.data.frame"))
            tbly <- paste0(content(y), " s2")
        else
            tbly <- paste0("(", content(y), ") s2")

        a <- paste0("\"", names(x), "\"")
        m <- as.integer(.db.getQuery(paste0("select array_upper(", a, ", 1) from ",
                                            tblx, " limit 1"), conn.id))
        b <- paste0("\"", names(y), "\"")
        n <- as.integer(.db.getQuery(paste0("select array_upper(", b, ", 1) from ",
                                            tbly, " limit 1"), conn.id))

        sql <- "array["
        for (j in seq_len(n)) {
            for (i in seq_len(m)) {
                sql <- paste(sql, "sum(s1.", a, "[", i, "] * s2.", b, "[", j, "])", sep = "")
                if (i != m) sql <- paste0(sql, ", ")
            }
            if (j != n) sql <- paste0(sql, ", ")
        }
        sql <- paste0(sql, "]")

        res <- new("db.Rquery",
                   .content = paste0("select ", sql, " as cross_prod from ", ),
                   )
    })
