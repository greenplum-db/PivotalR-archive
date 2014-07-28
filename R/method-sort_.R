
## -----------------------------------------------------------------------
## sort db.obj
## -----------------------------------------------------------------------

setGeneric ("sort")

setMethod (
    "sort",
    signature(x = "db.obj"),
    function (x, decreasing = FALSE, INDICES, ...)
    {
        if (is(x, "db.data.frame"))
            parent <- content(x)
        else
            parent <- x@.parent
        if (is.null(INDICES) ||
            (is.character(INDICES) && INDICES == "random")) {
            ## order by random()
            sort <- list(by = NULL, order = "",
                         str = " order by random()")
        } else {
            by <- character(0)
            if (!is.list(INDICES)) INDICES <- list(INDICES)
            for (i in seq_len(length(INDICES))) {
                if (!is(INDICES[[i]], "db.Rquery") ||
                    INDICES[[i]]@.parent != parent)
                    stop("Only objects derived from the same table can match each other!")
                by <- c(by, INDICES[[i]]@.expr)
            }
            by <- unique(by)

            if (length(by) > 1 && (length(decreasing) != 1 && length(decreasing) != length(by)))
                stop("How do you want to order each column? Specify 'decreasing'.")

            if (length(decreasing) == 1) decreasing <- rep(decreasing, length(by))
            order <- sapply(decreasing, function(b) if (b) "desc" else "asc")

            sort <- list(by = by, order = order,
                         str = paste(" order by ",
                         paste("(", by, ") ", order, collapse = ", ",
                               sep = ""), sep = ""))
        }

        if (is(x, "db.data.frame")) {
            content <- paste("select * from ", content(x),
                             sort$str, sep = "")
            expr <- paste("\"", names(x), "\"", sep = "")
            src <- content(x)
            parent <- src
            where <- ""
        } else {
            content <- paste(content(x), sort$str, sep = "")
            expr <- x@.expr
            src <- x@.source
            parent <- x@.parent
            where <- x@.where
        }

        new("db.Rquery",
            .content = content,
            .expr = expr,
            .source = src,
            .parent = parent,
            .conn.id = conn.id(x),
            .col.name = names(x),
            .key = x@.key,
            .col.data_type = x@.col.data_type,
            .col.udt_name = x@.col.udt_name,
            .where = where,
            .is.factor = x@.is.factor,
            .factor.ref = x@.factor.ref,
            .factor.suffix = x@.factor.suffix,
            .sort = sort,
            .dist.by = x@.dist.by)
    },
    valueClass = "db.Rquery")
