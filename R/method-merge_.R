## -----------------------------------------------------------------------
## Merge method
## -----------------------------------------------------------------------

setGeneric ("merge")

setMethod (
    "merge",
    signature(x = "db.obj", y = "db.obj"),
    function (x, y, by = intersect(names(x), names(y)),
              by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
              key = x@.key, suffixes = c("_x","_y"), ...) {
        if (!conn.eql(conn.id(x), conn.id(y)))
            stop("The two data sets are not on the same database!")

        if (is(x, "db.data.frame"))
            x.content <- paste(content(x), " s", sep = "")
        else if (is(x, "db.Rquery")) {
            x <- as.db.Rview(x)
            x.content <- paste("(", x@.parent, ") s", sep = "")
        }

        if (is(y, "db.data.frame"))
            y.content <- paste(content(y), " t", sep = "")
        else if (is(y, "db.Rquery")) {
            y <- as.db.Rview(y)
            y.content <- paste("(", y@.parent, ") t", sep = "")
        }

        if (length(by.x) != length(by.y))
            stop("The columns used to merge should have the same number!")

        on.str <- ""
        by.str <- NULL
        col.data_type <- NULL
        col.udt_name <- NULL
        is.factor <- NULL
        factor.ref <- NULL
        factor.suffix <- NULL
        if (length(by.x) == 0) {
            join.str <- "cross join"
        } else {
            on.str <- paste("on", paste(paste("s.\"", by.x, "\"", sep = ""),
                                        paste("t.\"", by.y, "\"", sep = ""),
                                        sep = " = ", collapse = " and "))
            if (all.x == FALSE && all.y == FALSE)
                join.str <- "join"
            else if (all.x == TRUE && all.y == FALSE)
                join.str <- "left join"
            else if (all.x == FALSE && all.y == TRUE)
                join.str <- "right join"
            else
                join.str <- "full join"

            by.str <- paste("s.\"", .strip(by.x, "\""), "\"", sep = "")
            col.idx <- .gwhich(names(x), by.x)
            col.data_type <- x@.col.data_type[col.idx]
            col.udt_name <- x@.col.udt_name[col.idx]

            col.idx.y <- .gwhich(names(y), by.y)
            is.factor <- x@.is.factor[col.idx] & y@.is.factor[col.idx.y]
            for (i in seq_len(length(is.factor))) {
                if (is.factor[i])
                    factor.ref <- c(factor.ref, x@.factor.ref[i])
                else
                    factor.ref <- c(factor.ref, as.character(NA))
            }
            factor.suffix <- rep("", length(is.factor))
            for (i in seq_len(length(factor.suffix))) # create new suffix
                if (is.factor[i]) factor.suffix[i] <- .unique.string.short()
        }

        parent <- paste(x.content, join.str, y.content, on.str)
        src <- parent

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
            factor.ref <- c(factor.ref, x@.factor.ref[idx])
            factor.suffix <- c(factor.suffix, x@.factor.suffix[idx])
            if (col %in% xy) {
                others <- c(others, paste("s.\"", col, "\"", sep = ""))
                col.name <- c(col.name, paste(col, suffixes[1], sep = ""))
            } else {
                others <- c(others, paste("\"", col, "\"", sep = ""))
                col.name <- c(col.name, col)
            }
        }
        for (col in y.diff) {
            idx <- which(names(y) == col)
            col.data_type <- c(col.data_type, y@.col.data_type[idx])
            col.udt_name <- c(col.udt_name, y@.col.udt_name[idx])
            is.factor <- c(is.factor, y@.is.factor[idx])
            factor.ref <- c(factor.ref, y@.factor.ref[idx])
            factor.suffix <- c(factor.suffix, y@.factor.suffix[idx])
            if (col %in% xy) {
                others <- c(others, paste("t.\"", col, "\"", sep = ""))
                col.name <- c(col.name, paste(col, suffixes[2], sep = ""))
            } else {
                others <- c(others, paste("\"", col, "\"", sep = ""))
                col.name <- c(col.name, col)
            }
        }

        if (!identical(key, character(0)) && key %in% xy) key <- paste(key, suffixes[1], sep = "")

        expr <- c(by.str, others)

        sql <- paste("select", paste(expr,
                                     paste("\"", col.name, "\"", sep = ""),
                                           sep = " as ", collapse = ", "),
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
            .is.factor = is.factor,
            .factor.ref = factor.ref,
            .factor.suffix = factor.suffix,
            .sort = list(by = "", order = "", str = ""),
            .dist.by = x@.dist.by)
    },
    valueClass = "db.Rquery")
