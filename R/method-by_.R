
## ------------------------------------------------------------------------
## by method
## ------------------------------------------------------------------------

setGeneric ("by")

setMethod (
    "by",
    signature(data = "db.obj", INDICES = "db.Rquery"),
    function (data, INDICES, FUN, ...) {
        if (is(data, "db.data.frame")) {
            parent <- content(data)
            src <- parent
            where <- ""
            where.str <- ""
        } else {
            parent <- data@.parent
            src <- data@.source
            where <- data@.where
            if (where != "") where.str <- paste("where", where)
        }
        by.name <- character(0)
        for (i in seq_len(length(INDICES))) {
            if (!is(INDICES[[i]], "db.Rquery") ||
                INDICES[[i]]@.parent != parent)
                stop("Only objects derived from the same table can match each other!")
            by.name <- c(by.name, names(INDICES[[i]]))
        }
        by.name <- unique(by.name)

        parent <- paste(parent, "group by", paste(by.name, collapse = ", "))

        tmp <- FUN(data)
        expr <- tmp@.expr
        col.name <- tmp@.col.name

        content <- paste("select",
                         paste(expr, col.name, sep = " as ",
                               collapse = ", "),
                         "from", parent, where.str)

        new("db.Rquery",
            .content = content,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(data),
            .col.name = col.name,
            .key = character(0),
            .col.data_type = tmp@.col.data_type,
            .col.udt_name = tmp@.col.udt_name,
            .where = where,
            .is.factor = rep(FALSE, length(names(data))),
            .sort = list(by = "", order = ""))
    },
    valueClass = "db.Rquery")
