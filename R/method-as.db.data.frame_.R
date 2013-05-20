
## ------------------------------------------------------------------------
## Convert other R objects into db.data.frame
## ------------------------------------------------------------------------

setGeneric (
    "as.db.data.frame",
    def = function (x, ...) standardGeneric("as.db.data.frame"),
    signature = "x")

## ------------------------------------------------------------------------

## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "data.frame"),
    def = function (
    x, table.name, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...)
    .method.as.db.data.frame.1(x, table.name, conn.id,
                               add.row.names, key,
                               distributed.by, is.temp, ...),
    valueClass = "db.data.frame")

## ------------------------------------------------------------------------

## put a file into a db.data.frame
## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "character"),
    def = function (
    x, table.name, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...)
    .method.as.db.data.frame.1(x, table.name, conn.id,
                               add.row.names, key,
                               distributed.by, is.temp, ...),
    valueClass = "db.data.frame")

## ------------------------------------------------------------------------

.method.as.db.data.frame.1 <- function (
    x, table.name, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...)
{
    if (!.is.arg.string(key)) stop("ID column name must be a string!")
    if (!identical(key, character(0)) &&
        key == "row.names" && !add.row.names)
        stop("Set row.names as TRUE if you want to use row.names as key!")
    ## argument default, and checking
    ## if (missing(conn.id)) conn.id <- 1
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such a connection!")
    if (!.is.arg.string(table.name) ||
        nchar(table.name) == 0)
        stop("The table name is not quite right!")
    ## if (missing(distributed.by)) distributed.by <- NULL
    ## if (missing(is.temp)) is.temp <- FALSE

    table <- .db.analyze.table.name(table.name)
    if ((!is.temp && .db.existsTable(table, conn.id)) ||
        (is.temp && .db.existsTempTable(table, conn.id)[[1]]))
        stop("Table already exists!")
    
    .db.writeTable(table, x, add.row.names = add.row.names,
                   distributed.by = distributed.by,
                   is.temp = is.temp, conn.id = conn.id, ...)
    if (length(table) == 1 && !is.temp) {
        table_schema <- .db.getQuery("select current_schema()");
        table.str <- paste(table_schema, ".", table, sep = "")
    } else
        table.str <- table.name
    if (! identical(key, character(0)))
        .db.getQuery(paste("alter table ", table.str,
                           " add primary key (\"",
                           key, "\")", sep = ""), conn.id)

    if (is.data.frame(x)) {
        cat("\nThe data in the data.frame", deparse(substitute(x)),
            "is stored into the table", table.name, "in database",
            dbname(conn.id), "on", host(conn.id), "!\n\n")
    } else {
        cat("\nThe data in the file", x,
            "is stored into the table", table.name, "in database",
            dbname(conn.id), "on", host(conn.id), "!\n\n")
    }
    cat("\nAn R object pointing to", table.name,
        "in database", dbname(conn.id), "on", host(conn.id),
        "is created !\n\n")
    
    db.data.frame(table.name, conn.id, key)
}

## ------------------------------------------------------------------------

## convert a db.Rquery object into a db.data.frame object

setMethod (
    "as.db.data.frame",
    signature (x = "db.Rquery"),
    def = function (x, table.name, conn.id = 1, is.view = FALSE,
    is.temp = FALSE, verbose = TRUE, pivot.factor = TRUE) {
        if (is.temp) 
            temp.str <- "temp"
        else
            temp.str <- ""
        if (is.view)
            obj.str <- "view"
        else
            obj.str <- "table"

        if (x@.parent == x@.source)
            tbl <- x@.parent
        else
            tbl <- paste("(", x@.parent, ")", sep = "")

        ## deal with factor, if exists
        ## We still need to keep the original non-factor
        ## column, because sometimes one wants to use the original
        ## data without regarding it as a factor. For example, as the
        ## grouping column.
        extra <- paste(x@.expr, collapse = ",")
        ## suffix used to avoid conflicts
        suffix <- rep("", seq_len(x@.is.factor))

        if (pivot.factor) {
            for (i in seq_len(x@.is.factor)) {
                if (x@.is.factor[i]) {
                    distinct <- .db.getQuery(paste("select distinc",
                                                x@.col.name[i],
                                                "from", tbl))[[1]]
                    suffix[i] <- .unique.string()
                    ## Produce a fixed order for distinct values
                    distinct <- distinct[order(distinct, decreasing = TRUE)]
                    for (j in seq_len(length(distinct) - 1)) {
                        new.col <- paste(x@.col.name[i], suffix[i],
                                        distinct[j], sep = "")
                        if (extra != "") extra <- paste(extra, ", ")
                        extra <- paste(extra, "(case when", x@.expr[i], "=",
                                    distinct[j], "then 1 else 0 end) as",
                                    new.col)
                    }
                } 
            }
        }

        if (x@.source == x@.parent)
            tbl <- x@.parent
        else
            tbl <- paste("(", x@.parent, ") s", sep = "")
        if (x@.where != "")
            where <- paste("where", x@.where)
        else
            where <- ""
        
        content.str <- paste("select", extra, "from", tbl, where)
                
        create.str <- paste("create ", temp.str, " ", obj.str, " ",
                            table.name,
                            " as (", content(x), ")", sep = "")
        .db.getQuery(create.str, conn.id) # create table
        res <- db.data.frame(table.name, conn.id, x@.key, verbose)
        res@.factor.suffix <- suffix
        res
    },
    valueClass = "db.data.frame")
