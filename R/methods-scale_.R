## Method: scale
## Description: Rescale the data table
## Returns: A db.Rquery object, also with attributes
## scaled:center and scaled:scale

setMethod (
    "scale",
    signature(x = "db.obj"),
    function (x, center = TRUE, scale = TRUE) {
        y <- rowAgg(x)
        names(y) <- "vec"
        col.dim <- length(strsplit(y@.expr, ",")[[1]])
        
        conn.id <- conn.id(y)
        madlib <- schema.madlib(conn.id)

        sql <- paste("select (f).*, n from (select ", madlib,
                     ".__utils_var_scales_result(", madlib,
                     ".utils_var_scales(vec, ", col.dim,
                     ")) as f, count(vec) as n from (",
                     content(y), ") h) s", sep = "")
        res <- .db.getQuery(sql, conn.id)
        avg <- as.vector(arraydb.to.arrayr(res$mean, "double"))
        n <- res$n # row dimension
        std <- (as.vector(arraydb.to.arrayr(res$std, "double")) *
                sqrt(n/(n-1)))

        std1 <- std
        std1[std == 0] <- 1

        z <- x - avg
        z <- z / std1
        names(z) <- names(x)
        attr(z, "scaled:center") <- avg
        attr(z, "scaled:scale") <- std
        z
    },
    valueClass = "db.Rquery")
