
## ------------------------------------------------------------------------
## by method
## ------------------------------------------------------------------------

setGeneric ("by")

setMethod (
    "by",
    signature(data = "db.obj"),
    function (data, INDICES = NULL, FUN, ...) {
        if (is(data, "db.data.frame")) {
            parent <- content(data)
            src <- parent
            where <- ""
            where.str <- ""
        } else {
            parent <- data@.parent
            src <- data@.source
            where <- data@.where
            if (where != "")
                where.str <- paste(" where", where)
            else
                where.str <- ""
        }

        if (!is.null(INDICES)) {
            by.name <- character(0)
            if (!is.list(INDICES)) INDICES <- list(INDICES)
            for (i in seq_len(length(INDICES))) {
                if (!is(INDICES[[i]], "db.Rquery") ||
                    INDICES[[i]]@.parent != parent)
                    stop("Only objects derived from the same table can match each other!")
                by.name <- c(by.name, names(INDICES[[i]]))
            }
            by.name <- unique(by.name)

            if (is(data, "db.data.frame") || data@.parent == data@.source)
                parent <- paste("\"", parent, "\"", sep = "")
            parent <- paste(parent, " group by ",
                            paste("\"", by.name, "\"", collapse = ", ",
                                  sep = ""),
                            sep = "")
            src <- parent
        }

        expr <- rep("", length(names(data)))
        col.name <- rep("", length(names(data)))
        col.data_type <- rep("", length(names(data)))
        col.udt_name <- rep("", length(names(data)))
        for (i in seq_len(length(names(data)))) {
            tmp <- FUN(data[[names(data)[i]]])
            expr[i] <- tmp@.expr
            col.name[i] <- tmp@.col.name
            col.data_type <- tmp@.col.data_type
            col.udt_name <- tmp@.col.udt_name
        }

        content <- paste("select ",
                         paste(expr, paste("\"", col.name, "\"", sep = ""),
                               sep = " as ",
                               collapse = ", "),
                         " from ", parent, where.str, sep = "")

        new("db.Rquery",
            .content = content,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(data),
            .col.name = col.name,
            .key = character(0),
            .col.data_type = col.data_type,
            .col.udt_name = col.udt_name,
            .where = where,
            .is.factor = rep(FALSE, length(names(data))),
            .sort = list(by = "", order = ""))
    },
    valueClass = "db.Rquery")
