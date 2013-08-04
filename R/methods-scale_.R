## Method: scale
## Description: Rescale the data table
## Returns: A db.Rquery object, also with attributes
## scaled:center and scaled:scale

setMethod (
    "scale",
    signature(x = "db.obj"),
    function (x, center = TRUE, scale = TRUE) {
        if (!is.numeric(center) && !is.logical(center) ||
            !is.numeric(scale) && !is.logical(scale))
            stop("center and scale must be numeric or logical !")
        
        conn.id <- conn.id(x)
        ## suppress all messages
        msg.level <- .set.msg.level("panic", conn.id) 
        ## disable warning in R, RPostgreSQL
        ## prints some unnessary warning messages
        warn.r <- getOption("warn")
        options(warn = -1)

        all.names <- names(.expand.array(x))
        lg <- length(all.names)
        y <- rowAgg(x)
        names(y) <- "vec"
        col.dim <- length(strsplit(y@.expr, ",")[[1]])
        
        madlib <- schema.madlib(conn.id)

        both.numeric <- 0
        if (is.numeric(center)) {
            both.numeric <- both.numeric + 1
            if (length(center) != lg)
                stop("The length of center vector must be equal to ",
                     "the number of columns of ", deparse(substitute(x)),
                     " (include array elements)!")
            avg1 <- center
        }
        if (is.numeric(scale)) {
            both.numeric <- both.numeric + 1
            if (length(scale) != lg)
                stop("The length of scale vector must be equal to ",
                     "the number of columns of ", deparse(substitute(x)),
                     " (include array elements)!")
            std1 <- scale
        }
        
        if (both.numeric != 2) {
            sql <- paste("select (f).*, n from (select ", madlib,
                         ".__utils_var_scales_result(", madlib,
                         ".utils_var_scales(vec, ", col.dim,
                         ")) as f, count(vec) as n from (",
                         content(y), ") h) s", sep = "")
            res <- .get.res(sql, conn.id = conn.id)
            n <- res$n # row dimension
            savg <- as.vector(arraydb.to.arrayr(res$mean, "double"))
        }

        if (is.logical(center)) {
            if (center)
                avg1 <- savg
            else
                avg1 <- rep(0, length(savg))
        }

        if (is.logical(scale)) {
            var <- (as.vector(arraydb.to.arrayr(res$std, "double"))^2 +
                    savg^2)
            if (scale)
                std1 <- sqrt(var - 2*avg1*savg+ avg1^2) * sqrt(n/(n-1))
            else
                std1 <- rep(1, length(savg))
        }

        std1[std1 == 0] <- 1

        z <- x - avg1
        z <- z / std1
        names(z) <- names(x)
        if (is.numeric(center) || center) {
            names(avg1) <- all.names
            attr(z, "scaled:center") <- avg1
        } else
            attr(z, "scaled:center") <- NULL
        if (is.numeric(scale) || scale) {
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
