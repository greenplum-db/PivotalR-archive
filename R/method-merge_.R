
## ------------------------------------------------------------------------
## Merge method
## ------------------------------------------------------------------------

setGeneric ("merge")

setMethod (
    "merge",
    signature(x = "db.obj", y = "db.obj"),
    function (x, y, by = intersect(names(x), names(y)),
              by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
              key = x@.key, suffixes = c("_x","_y"), ...) {
        if (!conn.eql(conn.id(x), conn.id(y)))
            stop("The two data sets are not on the same database!")
            
        if (is(x, "db.data.frame") || (is(x, "db.Rquery") &&
                                       x@.source == x@.parent))
            x.content = paste(content(x), "s")
        else
            x.content = paste("(", content(x), ") s", sep = "")
        if (is(y, "db.data.frame") || (is(y, "db.Rquery") &&
                                       y@.source == y@.parent))
            y.content = paste(content(y), "t")
        else
            y.content = paste("(", content(y), ") t", sep = "")

        if (length(by.x) != length(by.y))
            stop("The columns used to merge should have the same number!")

        if (length(by.x) == 0) {
            join.str <- "cross join"
            on.str <- ""
        } else {
            on.str <- paste("on", paste(paste("s.", by.x, sep = ""),
                                        paste("t.", by.y, sep = ""),
                                        sep = " = ", collapse = " and "))
            if (all.x == FALSE && all.y == FALSE) 
                join.str <- "join"
            else if (all.x == TRUE && all.y == FALSE)
                join.str <- "left join"
            else if (all.x == FALSE && all.y == TRUE)
                joi.str <- "right join"
            else
                join.str <- "full join"
        }

        parent <- paste(x.content, join.str, y.content, on.str)
        src <- parent

        by.str <- paste("s.", by.x, sep = "")
        col.idx <- .gwhich(names(x), by.x)
        col.data_type <- x@.col.data_type[col.idx]
        col.udt_name <- x@.col.udt_name[col.idx]

        col.idx.y <- .gwhich(names(y), by.y)
        is.factor <- x@.is.factor[col.idx] & y@.is.factor[col.idx.y]
        
        col.name <- by.x
        x.diff <- setdiff(names(x), by.x)
        y.diff <- setdiff(names(y), by.y)
        xy <- intersect(x.diff, y.diff)
        others <- character(0)
        for (col in x.diff) {
            idx <- which(names(x) == col)
            col.data_type <- c(col.data_type, x@.col.data_type[idx])
            col.udt_name <- c(col.udt_name, x@.col.udt_name[idx])
            is.factor <- c(is.factor, x@.is.factor[idx])
            if (col %in% xy) {
                others <- c(others, paste("s.", col, sep = ""))
                col.name <- c(col.name, paste(col, suffixes[1], sep = ""))
            } else {
                others <- c(others, col)
                col.name <- c(col.name, col)
            }
        }
        for (col in y.diff) {
            idx <- which(names(y) == col)
            col.data_type <- c(col.data_type, y@.col.data_type[idx])
            col.udt_name <- c(col.udt_name, y@.col.udt_name[idx])
            is.factor <- c(is.factor, y@.is.factor[idx])
            if (col %in% xy) {
                others <- c(others, paste("t.", col, sep = ""))
                col.name <- c(col.name, paste(col, suffixes[2], sep = ""))
            } else {
                others <- c(others, col)
                col.name <- c(col.name, col)
            }
        }

        if (key %in% xy) key <- paste(key, suffixes[1], sep = "")
        
        expr <- c(by.str, others)

        sql <- paste("select", paste(expr, col.name, sep = " as ", collapse = ", "),
                     "from", src)

        new("db.Rquery",
            .content = sql,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(x),
            .col.name = col.name,
            .key = key,
            .col.data_type = col.data_type,
            .col.udt_name = col.udt_name,
            .where = "",
            .is.factor = is.factor)
    },
    valueClass = "db.Rquery")
