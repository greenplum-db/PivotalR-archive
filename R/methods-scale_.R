## Method: scale
## Description: Rescale the data table
## Returns: A db.Rquery object, also with attributes
## scaled:center and scaled:scale

setMethod (
    "scale",
    signature(x = "db.obj"),
    function (x, center = TRUE, scale = TRUE) {
        conn.id <- conn.id(x)
        ## suppress all messages
        msg.level <- .set.msg.level("panic", conn.id) 
        ## disable warning in R, RPostgreSQL
        ## prints some unnessary warning messages
        warn.r <- getOption("warn")
        options(warn = -1)
        
        y <- rowAgg(x)
        names(y) <- "vec"
        col.dim <- length(strsplit(y@.expr, ",")[[1]])
        
        madlib <- schema.madlib(conn.id)

        sql <- paste("select (f).*, n from (select ", madlib,
                     ".__utils_var_scales_result(", madlib,
                     ".utils_var_scales(vec, ", col.dim,
                     ")) as f, count(vec) as n from (",
                     content(y), ") h) s", sep = "")
        res <- .get.res(sql, conn.id = conn.id)
        n <- res$n # row dimension
        avg <- as.vector(arraydb.to.arrayr(res$mean, "double"))
        if (center)
            avg1 <- avg
        else
            avg1 <- rep(0, length(avg))
        var <- (as.vector(arraydb.to.arrayr(res$std, "double"))^2 +
                avg^2)
        if (scale)
            std1 <- sqrt(var - avg1^2) * sqrt(n/(n-1))
        else
            std1 <- rep(1, length(avg))

        std1[std1 == 0] <- 1

        z <- x - avg1
        z <- z / std1
        names(z) <- names(x)
        all.names <- names(.expand.array(x))
        if (center) {
            names(avg) <- all.names
            attr(z, "scaled:center") <- avg
        } else
            attr(z, "scaled:center") <- NULL
        if (scale) {
            names(std1) <- all.names
            attr(z, "scaled:scale") <- std1
        } else
            attr(z, "scaled:scale") <- NULL

        ## reset message level
        msg.level <- .set.msg.level(msg.level, conn.id) 
        options(warn = warn.r) # reset R warning level
        z
    },
    valueClass = "db.Rquery")
