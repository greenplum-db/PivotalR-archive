
## ------------------------------------------------------------------------
## Convert other R objects into db.data.frame
## ------------------------------------------------------------------------

setGeneric (
    "as.db.data.frame",
    def = function (x, table.name, verbose = FALSE, ...) {
        x.str <- deparse(substitute(x))
        res <- standardGeneric("as.db.data.frame")
        if (verbose) {
            if (is.data.frame(x)) {
                cat("\nThe data in the data.frame", x.str,
                    "is stored into the table", table.name, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n\n")
            } else if (is.character(x)) {
                cat("\nThe data in the file", x.str,
                    "is stored into the table", table.name, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n\n")
            } else {
                cat("\nThe data created by ", x.str,
                    "is stored into the table", table.name, "in database",
                    dbname(res$conn.id), "on", host(res$conn.id), "!\n\n")
            }
        }
        return (res$res)
    },
    signature = "x")

## ------------------------------------------------------------------------

## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "data.frame"),
    def = function (
    x, table.name, verbose = FALSE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...) {
         .method.as.db.data.frame.1(x, 
                                   table.name, verbose, conn.id,
                                   add.row.names, key,
                                   distributed.by, is.temp, ...)
    })

## ------------------------------------------------------------------------

## put a file into a db.data.frame
## put a data.frame into a db.data.frame
setMethod (
    "as.db.data.frame",
    signature (x = "character"),
    def = function (
    x, table.name, verbose = FALSE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...) {
        .method.as.db.data.frame.1(x,
                                   table.name, verbose, conn.id,
                                   add.row.names, key,
                                   distributed.by, is.temp, ...)
    })

## ------------------------------------------------------------------------

.method.as.db.data.frame.1 <- function (
    x, table.name, verbose = FALSE, conn.id = 1, add.row.names = FALSE,
    key = character(0), distributed.by = NULL,
    is.temp = FALSE, ...)
{
    exists <- db.existsObject(table.name, conn.id, is.temp)
    if (is.temp) exists <- exists[[1]]
    if (exists) stop("The table already exists in connection ", conn.id, "!")
    
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
        table_schema <- .db.getQuery("select current_schema()", conn.id);
        table.str <- paste(table_schema, ".", table, sep = "")
    } else
        table.str <- table.name
    if (! identical(key, character(0)))
        .db.getQuery(paste("alter table ", table.str,
                           " add primary key (\"",
                           key, "\")", sep = ""), conn.id)

    ## cat("\nAn R object pointing to", table.name,
    ##     "in database", dbname(conn.id), "on", host(conn.id),
    ##     "is created !\n\n")
    
    list(res = db.data.frame(x = table.name, conn.id = conn.id, key = key,
         verbose = verbose, is.temp = is.temp),
         conn.id = conn.id)
}

## ------------------------------------------------------------------------

## convert a db.Rquery object into a db.data.frame object

setMethod (
    "as.db.data.frame",
    signature (x = "db.Rquery"),
    def = function (x, table.name, verbose = FALSE,
    is.view = FALSE,
    is.temp = FALSE,  pivot = TRUE,
    distributed.by = NULL, nrow = NULL) {
        conn.id <- conn.id(x)

        dist.str <- .get.distributed.by.str(conn.id, distributed.by)
        
        exists <- db.existsObject(table.name, conn.id, is.temp)
        if (is.temp) exists <- exists[[1]]
        if (exists) stop("The table already exists in connection ", conn.id, "!")
        
        if (is.temp) 
            temp.str <- "temp"
        else
            temp.str <- ""
        if (is.view)
            obj.str <- "view"
        else
            obj.str <- "table"

        if (x@.source == x@.parent)
            tbl <- x@.parent
        else
            tbl <- paste("(", x@.parent, ") s", sep = "")

        ## deal with factor, if exists
        ## We still need to keep the original non-factor
        ## column, because sometimes one wants to use the original
        ## data without regarding it as a factor. For example, as the
        ## grouping column.
        extra <- paste(x@.expr, paste("\"", names(x), "\"", sep = ""),
                       sep = " as ", collapse = ",")
        ## suffix used to avoid conflicts
        suffix <- x@.factor.suffix
        appear <- x@.col.name
        is.factor <- x@.is.factor
        
        if (pivot && !all(x@.is.factor == FALSE)) {
            cats <- x@.expr[x@.is.factor]
            sql <- "select "
            for (i in seq_len(length(cats))) {
                sql <- paste(sql, "array_agg(distinct ", cats[i], ") as ",
                             "distinct_", i, sep = "")
                if (i != length(cats)) sql <- paste(sql, ",", sep = "")
            }
            ## scan through the table only once
            sql <- paste(sql, " from ", tbl, sep = "")
            distincts <- .db.getQuery(sql, conn.id)
            idx <- 0
            dummy <- character(0)
            dummy.expr <- character(0)
            for (i in seq_len(length(x@.is.factor))) {
                if (x@.is.factor[i]) {
                    idx <- idx + 1
                    distinct <- as.vector(arraydb.to.arrayr(distincts[[paste("distinct_",idx,sep="")]], "character"))
                    ## Produce a fixed order for distinct values
                    distinct <- distinct[order(distinct, decreasing = TRUE)]
                    for (j in seq_len(length(distinct) - 1)) {
                        new.col <- paste(x@.col.name[i], suffix[i],
                                        distinct[j], sep = "")
                        is.factor <- c(is.factor, FALSE)
                        if (extra != "") extra <- paste(extra, ", ")
                        dex <- paste("(case when ", x@.expr[i], " = '",
                                     distinct[j], "'::", x@.col.data_type[i],
                                     " then 1 else 0 end)", sep = "")
                        extra <- paste(extra, " ", dex, " as ",
                                       "\"", new.col, "\"", sep = "")
                        appear <- c(appear, paste(x@.col.name[i],":",
                                                  distinct[j], sep = ""))
                        dummy <- c(dummy, new.col)
                        dummy.expr <- c(dummy.expr, dex)
                    }
                } 
            }
        }
        
        if (x@.where != "")
            where <- paste(" where", x@.where)
        else
            where <- ""

        if (!is.null(nrow))
            nrow.str <- paste(" limit ", nrow, " ", sep = "")
        else
            nrow.str <- ""
        
        content.str <- paste("select ", extra, " from ", tbl, where, sep = "")
                
        create.str <- paste("create ", temp.str, " ", obj.str, " \"",
                            table.name,
                            "\" as (", content.str, nrow.str, ") ", dist.str, sep = "")

        .db.getQuery(create.str, conn.id) # create table
        ## print(create.str)
        res <- db.data.frame(x = table.name, conn.id = conn.id, key = x@.key,
                             verbose = verbose, is.temp = is.temp)
        res@.is.factor <- is.factor
        res@.factor.suffix <- suffix
        res@.appear.name <- appear
        res@.dummy <- dummy
        res@.dummy.expr <- dummy.expr
        list(res = res, conn.id = conn.id)
    })
