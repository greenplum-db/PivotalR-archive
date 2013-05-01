
## ------------------------------------------------------------------------
## create a R object that points to something inside the database
## ------------------------------------------------------------------------

db.data.frame <- function (x, conn.id = 1, key = character(0))
{
    if (! .is.arg.string(x))
        stop("The name of the database object must be a string!")
    if (! .is.conn.id.valid(conn.id))
        stop("There is no such a connection to any database!")

    ## a vector (schema_name, table_name) or just table_name
    table <- .db.analyze.table.name(x) 
    if (!.is.table.or.view(table, conn.id))
        stop("No such table or view!")

    if (.is.view(table, conn.id))
    {
        ## view
        res <- new("db.view",
                   .name = table,
                   .content = x,
                   .conn.id = conn.id,
                   .key = character(0))
    }
    else
    {
        ## table
        res <- new("db.table",
                   .name = table,
                   .content = x,
                   .conn.id = conn.id,
                   .key = key)
    }

    col.info <- .db.getQuery(
        paste("select column_name, data_type, udt_name from information_schema.columns where ",
              .db.table.schema.str(table), sep = ""), conn.id)

    res@.col.name <- col.info$column_name
    res@.col.data_type <- col.info$data_type
    res@.col.udt_name <- col.info$udt_name

    if (is(res, "db.table")) {
        ## compute dim
        col.num <- length(res@.col.name)
        row.num <- .db.getQuery(paste("select count(*) from", x), conn.id)
        res@.dim <- c(row.num$count, col.num)
    }

    ## table type (local temp)
    tbl.type <- .db.getQuery(
        paste("select table_type from information_schema.tables where ",
              .db.table.schema.str(table), sep = ""), conn.id)
    res@.table.type <- tbl.type$table_type

    cat("\nAn R object pointing to", x,
        "in database", dbname(conn.id), "on", host(conn.id),
        "is created !\n\n")
    
    return (res)
}

