
## -----------------------------------------------------------------------
## by method
## -----------------------------------------------------------------------

setGeneric ("by")

setMethod (
    "by",
    signature(data = "db.obj"),
    function (data, INDICES, FUN, ..., simplify = TRUE) {
        if (is.list(INDICES))
            ## indx <- Reduce(cbind2, INDICES[-1], INDICES[[1]])
            indx <- .combine.list(INDICES)
        else
            indx <- INDICES
        v <- lk(indx, 1, array = FALSE)
        add.quotes <- function (str)
            unlist(Map(function (x)
                       if (is.character(x)) paste("\"", x, "\"", sep = "")
                       else x, str))
        v <- add.quotes(v)
        get.piece <- function (x, indx, v) {
            s <- paste(gsub("\"", "`", indx@.expr), "==", v, sep = "",
                       collapse = " && ")
            for (i in which(is.na(v)))
                s <- gsub(paste(gsub("\"", "`", indx@.expr[i]), "==", v[i],
                                sep = ""),
                          paste("is.na(", indx@.expr[i], ")", sep = ""), s)
            eval(parse(text = paste("with(x, x[", s, ",])",
                       sep = "")))
        }
        use <- get.piece(data, indx, v)
        fit0 <- FUN(use)
        if (!is(fit0, "db.obj")) {
            vals <- lk(unique(db.array(as.character(indx, array = FALSE))))
            if (!is.data.frame(vals))
                vals <- data.frame(V1 = vals, stringsAsFactors = F)
            rst <- list(list(index = NULL, result = fit0))
            count <- 1
            for (i in seq_len(nrow(vals))) {
                w <- add.quotes(vals[i,])
                if (all(w == v)) {
                    rst[[1]]$index <- vals[i,]
                    next
                }
                use <- get.piece(data, indx, w)
                rst[[count+1]] <- list(index = vals[i,], result = FUN(use))
                count <- count + 1
            }
            return (rst)
        } else {
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

            grp.expr <- character(0)
            grp.col.name <- character(0)
            grp.col.data_type <- character(0)
            grp.col.udt_name <- character(0)
            if (!is.null(INDICES)) {
                by.name <- character(0)
                if (!is.list(INDICES)) INDICES <- list(INDICES)
                for (i in seq_len(length(INDICES))) {
                    if (!is(INDICES[[i]], "db.Rquery") ||
                        INDICES[[i]]@.parent != parent)
                        stop("Only objects derived from the same table can match each other!")
                    for (j in seq_len(length(INDICES[[i]]@.expr))) {
                        if (! INDICES[[i]]@.expr[j] %in% by.name) {
                            by.name <- c(by.name, INDICES[[i]]@.expr[j])
                            grp.col.name <- c(grp.col.name, INDICES[[i]]@.col.name[j])
                            grp.col.data_type <- c(grp.col.data_type, INDICES[[i]]@.col.data_type[j])
                            grp.col.udt_name <- c(grp.col.udt_name, INDICES[[i]]@.col.udt_name[j])
                        }
                    }
                }
                ## by.name <- unique(by.name)
                grp.expr <- by.name

                parent <- paste(parent, where.str, " group by ",
                                paste("(", by.name, ")", collapse = ", ",
                                      sep = ""),
                                sep = "")
                where.str <- ""
                where <- ""
                src <- parent
            }

            tmp <- FUN(data)

            expr <- tmp@.expr
            col.name <- tmp@.col.name
            col.data_type <- tmp@.col.data_type
            col.udt_name <- tmp@.col.udt_name

            expr <- c(grp.expr, expr)
            col.name <- c(grp.col.name, col.name)
            col.data_type <- c(grp.col.data_type, col.data_type)
            col.udt_name <- c(grp.col.udt_name, col.udt_name)

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
                .factor.ref = rep(as.character(NA), length(names(data))),
                .factor.suffix = rep("", length(names(data))),
                .sort = list(by = "", order = "", str = ""),
                .dist.by = data@.dist.by)
        }
    })
