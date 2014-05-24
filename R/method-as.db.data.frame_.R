## -----------------------------------------------------------------------
## Convert other R objects into db.data.frame
## -----------------------------------------------------------------------

setGeneric (
    "as.db.data.frame",
    def = function (x, table.name = NULL, verbose = TRUE, ...) {
        x.str <- deparse(substitute(x))
        res <- standardGeneric("as.db.data.frame")
        tst <- paste(res$res@.name, collapse = ".")
        if (verbose) {
            if (is.data.frame(x)) {
                cat("\nThe data in the data.frame", x.str,
                    "is stored into", tst, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n")
            } else if (is.character(x)) {
                cat("\nThe data in the file", x.str,
                    "is stored into", tst, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n")
            } else if (is(x, "db.Rquery")) {
                cat("\nThe data created by", x.str,
                    "is stored into", tst, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n\n")
            } else {
                cat("\nThe data contained in table", content(x),
                    "which is wrapped by", x.str,
                    "is copied into", tst, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n\n")
            }
        }

        return (res$res)
    },
    signature = "x")

## -----------------------------------------------------------------------

## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "data.frame"),
    def = function (
    x, table.name = NULL, verbose = TRUE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL, append = FALSE,
    is.temp = FALSE, ...) {
        if (is.null(table.name)) {
            table.name <- .unique.string()
            is.temp <- TRUE
        }
        .method.as.db.data.frame.1(x,
                                   table.name, verbose, conn.id,
                                   add.row.names, key,
                                   distributed.by, append, is.temp, ...)
    })

## -----------------------------------------------------------------------

## put a file into a db.data.frame
## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "character"),
    def = function (
    x, table.name = NULL, verbose = TRUE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL, append = FALSE,
    is.temp = FALSE, ...) {
        if (is.null(table.name)) {
            table.name <- .unique.string()
            is.temp <- TRUE
        }

        f <- paste(getwd(), "/", x, sep = "")
        if (file.exists(f)) x <- f
        else if (!file.exists(x))
            stop("the file does not exist!")
        .method.as.db.data.frame.1(x,
                                   table.name, verbose, conn.id,
                                   add.row.names, key,
                                   distributed.by, append, is.temp, ...)
    })

## -----------------------------------------------------------------------

.method.as.db.data.frame.1 <- function (
    x, table.name = NULL, verbose = TRUE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL, append = FALSE,
    is.temp = FALSE, ...)
{
    if (!.is.conn.id.valid(conn.id))
        stop("Connection ID ", conn.id, " is not valid!")
    warnings <- .suppress.warnings(conn.id)
    if (is.null(table.name)) {
        table.name <- .unique.string()
        is.temp <- TRUE
    }

    exists <- db.existsObject(table.name, conn.id, is.temp)
    if (is.temp) exists <- exists[[1]]
    if (exists) {
        if (!append) {
            .restore.warnings(warnings)
            stop("The table already exists in connection ", conn.id, "!")
        }
    } else {
        if (append) {
            .restore.warnings(warnings)
            stop("The table to be appended does not exist in connection ", conn.id, "!")
        }
    }

    if (!.is.arg.string(key)) stop("ID column name must be a string!")
    if (!identical(key, character(0)) &&
        key == "row.names" && !add.row.names) {
        .restore.warnings(warnings)
        stop("Set row.names as TRUE if you want to use row.names as key!")
    }
    ## argument default, and checking
    ## if (missing(conn.id)) conn.id <- 1
    if (!.is.conn.id.valid(conn.id)) {
        .restore.warnings(warnings)
        stop("There is no such a connection!")
    }
    if (!.is.arg.string(table.name) ||
        nchar(table.name) == 0) {
        .restore.warnings(warnings)
        stop("The table name is not quite right!")
    }
    ## if (missing(distributed.by)) distributed.by <- NULL
    ## if (missing(is.temp)) is.temp <- FALSE

    table <- .db.analyze.table.name(table.name)

    if (!append && ((!is.temp && .db.existsTable(table, conn.id)) ||
        (is.temp && .db.existsTempTable(table, conn.id)[[1]]))) {
        .restore.warnings(warnings)
        stop("Table already exists!")
    }

    .db.writeTable(table, x, add.row.names = add.row.names,
                   distributed.by = distributed.by, append = append,
                   is.temp = is.temp, conn.id = conn.id, ...)

    if (length(table) == 1 && !is.temp) {
        table_schema <- db.q("select current_schema()", conn.id = conn.id, verbose = FALSE);
        table.str <- paste(table_schema, ".", table, sep = "")
    } else
        table.str <- table.name
    if (! identical(key, character(0))) {
        db <- .get.dbms.str(conn.id)
        if (db$db.str == "HAWQ") {
            .restore.warnings(warnings)
            stop("HAWQ does not support primary keys!")
        }
        db.q("alter table ", table.str,
             " add primary key (\"",
             key, "\")", sep = "",
             conn.id = conn.id, verbose = FALSE)
    }

    .restore.warnings(warnings)

    tbn <- strsplit(table.name, "\\.")[[1]]
    tbnn <- paste("\"", .strip(tbn, "\""),
                  "\"", collapse = ".", sep = "")

    list(res = db.data.frame(x = tbnn, conn.id = conn.id, key = key,
         verbose = verbose, is.temp = is.temp),
         conn.id = conn.id)
}

## -----------------------------------------------------------------------

## Use this to represent NULL
## user is extremely unlikely to use this name
.null.string <- "<@#$NULL%^&>"

## convert a db.Rquery object into a db.data.frame object

setMethod (
    "as.db.data.frame",
    signature (x = "db.Rquery"),
    def = function (x, table.name = NULL, verbose = TRUE,
    is.view = FALSE,
    is.temp = FALSE,  pivot = TRUE,
    distributed.by = NULL, nrow = NULL, field.types = NULL,
    na.as.level = FALSE, # whether use NULL as a level
    factor.full = rep(FALSE, length(names(x)))) { # whether expand all levels
        warnings <- .suppress.warnings(conn.id(x))

        if (is.null(table.name)) {
            table.name <- .unique.string()
            is.temp <- TRUE
        }

        conn.id <- conn.id(x)

        if (is.view || !all(distributed.by %in% x@.col.name))
            dist.str <- ""
        else
            dist.str <- .get.distributed.by.str(conn.id, distributed.by)
        exists <- db.existsObject(table.name, conn.id, is.temp)

        if (is.temp) exists <- exists[[1]]
        if (exists) {
            .restore.warnings(warnings)
            stop("The table already exists in connection ",
                 conn.id, "!")
        }

        if (is.temp)
            temp.str <- "temp"
        else
            temp.str <- ""
        if (is.view)
            obj.str <- "view"
        else
            obj.str <- "table"

        if (x@.where != "")
            where <- paste(" where", x@.where)
        else
            where <- ""

        if (x@.source == x@.parent)
            tbl <- x@.parent
        else
            tbl <- paste("(", x@.parent, ") s", sep = "")

        if (x@.where != "")
            where <- paste(" where", x@.where)
        else
            where <- ""

        ## deal with factor, if exists
        ## We still need to keep the original non-factor
        ## column, because sometimes one wants to use the original
        ## data without regarding it as a factor. For example, as the
        ## grouping column.
        if (is.null(field.types)) {
            data.types <- x@.col.data_type
            extra <- paste(x@.expr,
                           paste("\"", names(x), "\"", sep = ""),
                           sep = " as ", collapse = ",")
        } else {
            data.types <- character(0)
            for (i in names(x)) {
                if (is.null(field.types[[i]])) {
                    .restore.warnings(warnings)
                    stop("field.types should include all column types!")
                }
                data.types <- c(data.types, field.types[[i]])
            }
            extra <- paste(paste("(", x@.expr, ")::", data.types, sep = ""),
                           paste("\"", names(x), "\"", sep = ""),
                           sep = " as ", collapse = ",")
        }

        ## suffix used to avoid conflicts
        suffix <- x@.factor.suffix
        appear <- x@.col.name
        is.factor <- x@.is.factor
        factor.ref <- x@.factor.ref

        dummy <- character(0)
        dummy.expr <- character(0)
        factor.ref <- rep(as.character(NA), length(x@.is.factor))
        if (pivot && !all(x@.is.factor == FALSE)) {
            cats <- x@.expr[x@.is.factor]
            for (i in seq_len(length(x@.is.factor))) {
                if (x@.is.factor[i]) {
                    distinct <- lk(by(x[[i]], x[[i]], identity), -1)[,1]
                    if (na.as.level)
                        distinct[is.na(distinct)] <- .null.string
                    else
                        distinct <- distinct[!is.na(distinct)]

                    distinct <- .strip(distinct[order(distinct, decreasing = TRUE)], "\"")

                    if (is.na(x@.factor.ref[i]))
                        avoid <- distinct[length(distinct)]
                    else
                        avoid <- x@.factor.ref[i]
                    factor.ref[i] <- avoid
                    for (j in seq_len(length(distinct))) {
                        if (distinct[j] == avoid && !factor.full[i]) next
                        new.col <- paste(x@.col.name[i], suffix[i],
                                        distinct[j], sep = "")
                        is.factor <- c(is.factor, FALSE)
                        factor.ref <- c(factor.ref, as.character(NA))
                        if (extra != "") extra <- paste(extra, ", ")
                        dex <- paste("(case when (", x@.expr[i], ")::text = '",
                                     distinct[j], "'",
                                     " then 1 else 0 end)", sep = "")
                        extra <- paste(extra, " ", dex, " as ",
                                       "\"", new.col, "\"", sep = "")
                        appear <- c(appear, paste(x@.col.name[i],".",
                                                  if (distinct[j] != .null.string) distinct[j]
                                                  else "<NA>", sep = ""))
                        dummy <- c(dummy, new.col)
                        dummy.expr <- c(dummy.expr, dex)
                    }
                }
            }
        }

        if (!is.null(nrow))
            nrow.str <- paste(" limit ", nrow, " ", sep = "")
        else
            nrow.str <- ""

        content.str <- paste("select ", extra, " from ", tbl, where,
                             x@.sort$str,
                             sep = "")

        tbn <- strsplit(table.name, "\\.")[[1]]
        tbnn <- paste("\"", .strip(tbn, "\""),
                      "\"", collapse = ".", sep = "")

        create.str <- paste("create ", temp.str, " ", obj.str, " ",
                            tbnn,
                            " as (", content.str, nrow.str, ") ",
                            dist.str, sep = "")

        .restore.warnings(warnings)

        db.q(create.str, conn.id = conn.id, verbose = FALSE) # create table

        res <- db.data.frame(x = tbnn, conn.id = conn.id, key = x@.key,
                             verbose = verbose, is.temp = is.temp)
        res@.is.factor <- is.factor
        res@.factor.ref <- factor.ref
        res@.factor.suffix <- suffix
        res@.appear.name <- appear
        res@.dummy <- dummy
        res@.dummy.expr <- dummy.expr
        list(res = res, conn.id = conn.id)
    })

## -----------------------------------------------------------------------

## Make a copy of a table/view

setMethod (
    "as.db.data.frame",
    signature (x = "db.data.frame"),
    def = function (x, table.name = NULL, verbose = TRUE,
    is.view = FALSE, is.temp = FALSE,
    distributed.by = NULL, nrow = NULL, field.types = NULL) {
        if (is.null(table.name)) {
            table.name <- .unique.string()
            is.temp <- TRUE
        }

        tbn <- strsplit(table.name, "\\.")[[1]]
        tbnn <- paste("\"", .strip(tbn, "\""),
                      "\"", collapse = ".", sep = "")

        if (tbnn == content(x))
            stop("cannot copy an object into itself!")
        list(res = as.db.data.frame(x[,], tbnn, FALSE,
             is.view, is.temp, FALSE, distributed.by, nrow, field.types),
             conn.id = conn.id(x))
    })
