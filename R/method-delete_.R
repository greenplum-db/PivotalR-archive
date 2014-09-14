
## -----------------------------------------------------------------------
## delete db.data.frame objects
## -----------------------------------------------------------------------

setGeneric (
    "delete",
    def = function (x, ...) {
        rst <- standardGeneric("delete")
        res <- rst$res
        conn.id <- rst$conn.id
        if (res && (!is.character(x) ||
                    is.character(x) &&
                    .strip(deparse(substitute(x)), "\"") != x)) {
            envir <- parent.env(parent.env(parent.env(parent.env(
                parent.env(as.environment(-1))))))

            warn.r <- getOption("warn")
            options(warn = -1)
            rm(list=deparse(substitute(x)), envir=envir)
            options(warn = warn.r)
        }
        res
    },
    signature = "x")

## -----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "db.data.frame"),
    def = function (x, cascade = FALSE) {
        ## .db.removeTable(content(x), conn.id(x))
        if (x@.table.type == "LOCAL TEMPORARY")
            tbl <- gsub("^\\\"[^\"]*\\\"\\.", "", content(x))
        else
            tbl <- content(x)
        s <- delete(tbl, conn.id(x), x@.table.type == "LOCAL TEMPORARY",
                    cascade)
        list(res=s, conn.id=conn.id(x))
    })

## -----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "db.Rquery"),
    def = function (x) {
        list(res=TRUE, conn.id=conn.id(x))
    })

## -----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "character"),
    def = function (x, conn.id = 1, is.temp = FALSE, cascade = FALSE) {
        x <- paste("\"", .strip(strsplit(x, "\\.")[[1]], "\""), "\"",
                   collapse = ".", sep = "")
        if (is.temp)
            x <- gsub("^\\\"[^\"]*\\\"\\.", "", x)
        origin.x <- x
        warn.r <- getOption("warn")
        options(warn = -1)

        exists <- db.existsObject(x, conn.id, is.temp)
        if (length(exists) == 2)
            if (! exists[[1]]) {
                options(warn = warn.r) # reset R warning level
                return (list(res=FALSE, conn.id=conn.id))
            } else
                x <- exists[[2]]
        else
            if (! exists) {
                options(warn = warn.r) # reset R warning level
                return (list(res=FALSE, conn.id=conn.id))
            } else {
                if (length(x) == 1) x <- strsplit(x, "\\.")[[1]]
                if (length(x) != 2) {
                    schemas <- arraydb.to.arrayr(
                        .db.getQuery("select current_schemas(True)", conn.id),
                        type = "character")
                    table_schema <- character(0)
                    for (schema in schemas)
                        if (.db.existsTable(c(schema, x), conn.id))
                            table_schema <- c(table_schema, schema)
                    if (identical(table_schema, character(0))) {
                        options(warn = warn.r) # reset R warning level
                        return (list(res=FALSE, conn.id=conn.id))
                    }
                    schema.str <- strsplit(table_schema, "_")
                    for (i in seq_len(length(schema.str))) {
                        str <- schema.str[[i]]
                        if (str[1] != "pg" || str[2] != "temp") {
                            x <- c(table_schema[i], x)
                            break
                        }
                    }
                }
                if (length(x) == 1) {
                    options(warn = warn.r) # reset R warning level
                    return (list(res=FALSE, conn.id=conn.id))
                }
            }
        ## .db.removeTable(x, conn.id)
        table <- paste("\"", .strip(x[1], "\""),
                       "\".\"", .strip(x[2], "\""), "\"", sep = "")
        if (cascade) cascade.str <- " cascade"
        else cascade.str <- ""

        if (.is.view(x, conn.id))
            type.str <- "view "
        else
            type.str <- "table "
        sql <- paste("drop ", type.str, table, cascade.str, sep = "")
        res <- tryCatch(.db.getQuery(sql, conn.id),
                        error = function(e) { success <<- FALSE })
        exists <- db.existsObject(origin.x, conn.id, is.temp)

        options(warn = warn.r) # reset R warning level
        if (length(exists) == 2)
            if (! exists[[1]])
                return (list(res=TRUE, conn.id=conn.id))
            else
                return (list(res=FALSE, conn.id=conn.id))
        else
            if (! exists)
                return (list(res=TRUE, conn.id=conn.id))
            else
                return (list(res=FALSE, conn.id=conn.id))
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "arima.css.madlib"),
    def = function (x) {
        conn.id <- conn.id(x$model)
        d1 <- delete(x$model)
        d2 <- delete(x$residuals)
        d3 <- delete(x$statistics)
        if (x$temp.source) d4 <- delete(x$series)
        else d4 <- TRUE
        list(res=all(c(d1, d2, d3)), conn.id=conn.id)
    })


## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "summary.madlib"),
    def = function (x) {
        tbl <- attr(x, "summary")
        conn.id <- conn.id(tbl)
        d1 <- delete(tbl)
        list(res=d1, conn.id=conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "lm.madlib"),
    def = function (x) {
        if (is.null(x$model)) return (list(res=TRUE, conn.id=NULL))
        conn.id <- conn.id(x$model)
        d1 <- delete(x$model)
        list(res=d1, conn.id=conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "lm.madlib.grps"),
    def = function (x) {
        if (is.null(x[[1]]$model)) return (list(res=TRUE, conn.id=NULL))
        conn.id <- conn.id(x[[1]]$model)
        d1 <- delete(x[[1]]$model)
        list(res=d1, conn.id=conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "logregr.madlib"),
    def = function (x) {
        if (is.null(x$model)) return (list(res=TRUE, conn.id=NULL))
        conn.id <- conn.id(x$model)
        d1 <- delete(x$model)
        list(res=d1, conn.id=conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "logregr.madlib.grps"),
    def = function (x) {
        if (is.null(x[[1]]$model)) return (list(res=TRUE, conn.id=NULL))
        conn.id <- conn.id(x[[1]]$model)
        d1 <- delete(x[[1]]$model)
        list(res=d1, conn.id=conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "bagging.model"),
    def = function (x) {
        conn.id <- conn.id(x[[1]]$model)
        res <- lapply(x, delete)
        list(res = all(unlist(res)), conn.id = conn.id)
    })

## ----------------------------------------------------------------------

setMethod (
    "delete",
    signature (x = "elnet.madlib"),
    def = function (x) {
        if (is(x$model, "db.obj")) {
            conn.id <- conn.id(x$model)
            d1 <- delete(x$model)
        } else {
            conn.id <- NA
            d1 <- TRUE
        }
        list(res = d1, conn.id = conn.id)
    })

## ------------------------------------------------------------

setMethod("delete",
          signature(x = "dt.madlib"),
          def = function (x) {
              conn.id <- conn.id(x$model)
              success <- delete(x$model) && delete(x$model.summary)
              list(res = success, conn.id = conn.id)
          })

setMethod("delete",
          signature(x = "dt.madlib.grps"),
          def = function (x) {
              conn.id <- conn.id(x[[1]]$model)
              success <- delete(x[[1]]$model) && delete(x[[1]]$model.summary)
              list(res = success, conn.id = conn.id)
          })
