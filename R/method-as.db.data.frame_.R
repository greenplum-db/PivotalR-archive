
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
                   is.temp = is.temp, ...)
    if (length(table) == 1 && !is.temp) {
        table_schema <- .db.getQuery("select current_schema()");
        table.str <- paste(table_schema, ".", table, sep = "")
    } else
        table.str <- table.name
    if (! identical(key, character(0)))
        .db.getQuery(paste("alter table ", table.str,
                           " add primary key (\"",
                           key, "\")", sep = ""))

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

setMethod (
    "as.db.data.frame",
    signature (x = "db.Rquery"),
    def = function (x, table.name, conn.id, is.temp = FALSE) {
        stop("To be implemented !")
    },
    valueClass = "db.data.frame")
