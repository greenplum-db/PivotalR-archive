
## ------------------------------------------------------------------------
## sort db.obj
## ------------------------------------------------------------------------

setGeneric ("sort")

setMethod (
    "sort",
    signature(x = "db.obj"),
    function (x, decreasing = FALSE, by = character(0), ...)
    {
        if (is.null(by)) { # order by random()
            sort <- list(by = NULL, order = "",
                         str = "order by random()")
        } else {
            if (identical(by, character(0)) && !identical(x@.key, character(0)))
                by <- x@.key
            
            if (!is.character(by) || length(by) == 0 ||
                !all(by %in% names(x)))
                stop("must sort by the column names!")
            if (decreasing)
                order <- "desc"
            else
                order <- ""

            sort <- list(by = by, order = order,
                         str = paste(" order by ",
                         paste("\"", by, "\"", collapse = ", ",
                               sep = ""), order, sep = ""))
        }

        if (is(x, "db.data.frame")) {
            content <- paste("select * from ", content(x),
                             sort$str, sep = "")
            expr <- names(x)
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
            .sort = sort)
    },
    valueClass = "db.Rquery")
