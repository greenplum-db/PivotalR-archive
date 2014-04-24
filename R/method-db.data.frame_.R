## -----------------------------------------------------------------------
## create a R object that points to something inside the database
## -----------------------------------------------------------------------

db.data.frame <- function (x, conn.id = 1, key = character(0), verbose = TRUE,
                           is.temp = FALSE)
{
    if (! .is.arg.string(x))
        stop("The name of the database object must be a string!")
    if (!.is.conn.id.valid(conn.id))
        stop("Connection ID ", conn.id, " is not valid!")
    warnings <- .suppress.warnings(conn.id)

    tbn <- strsplit(x, "\\.")[[1]]
    x <- paste("\"", .strip(tbn, "\""),
               "\"", collapse = ".", sep = "")

    ## a vector (schema_name, table_name) or just table_name
    table <- .db.analyze.table.name(x)
    exists <- db.existsObject(x, conn.id, is.temp)
    if (is.temp) {
        table <- exists[[2]]
        exists <- exists[[1]]
    }

    if (!exists) {
        .restore.warnings(warnings)
        stop("No such object in the connection ", conn.id)
    }

    if (length(table) == 1)
        content <- paste("\"", .strip(table, "\""), "\"", sep = "")
    else
        content <- paste("\"", .strip(table, "\""),
                         "\"", sep = "", collapse = ".")

    if (.is.view(table, conn.id))
    {
        ## view
        res <- new("db.view",
                   .name = table,
                   .content = content,
                   .conn.id = conn.id,
                   .key = character(0),
                   .dist.by = character(0))
    }
    else
    {
        dbms <- (.get.dbms.str(conn.id))$db.str
        if (dbms != "PostgreSQL") {
            dist.cols <- .get.dist.policy(table, conn.id)
            if (is.na(dist.cols)) {
                dist.by <- character(0) # distributed randomly
            } else {
                dist.by <- paste(dist.cols, collapse = ", ")
            }
        } else {
            dist.str <- ""
            dist.by <- ""
        }
        ## table
        res <- new("db.table",
                   .name = table,
                   .content = content,
                   .conn.id = conn.id,
                   .key = key,
                   .dist.by = dist.by)
    }

    col.info <- .db.getQuery(
        paste("select column_name, data_type, udt_name from information_schema.columns where ",
              .db.table.schema.str(table, conn.id), " order by ordinal_position", sep = ""), conn.id)

    res@.col.name <- col.info$column_name
    if (!identical(res@.key, character(0)) &&
        (! res@.key %in% res@.col.name)) {
        .restore.warnings(warnings)
        stop("The key column does not exist!")
    }
    res@.col.data_type <- tolower(col.info$data_type)
    res@.col.udt_name <- tolower(col.info$udt_name)

    if (is(res, "db.table")) {
        ## compute dim
        col.num <- length(res@.col.name)
        if (verbose) cat("Counting and caching the data table dimension ... ")
        timing <- system.time({row.num <- db("select count(*) from", x, conn.id = conn.id, verbose = FALSE)})
        if (verbose) cat(as.numeric(timing[3]), "sec ... done.\n")
        res@.dim <- c(row.num$count, col.num)
    }

    ## table type (local temp)
    tbl.type <- .db.getQuery(
        paste("select table_type from information_schema.tables where ",
              .db.table.schema.str(table, conn.id), sep = ""), conn.id)

    res@.table.type <- tbl.type$table_type

    res@.is.factor <- rep(FALSE, length(res@.col.name))
    res@.factor.ref <- rep(as.character(NA), length(res@.col.name))
    res@.appear.name <- res@.col.name
    res@.factor.suffix <- rep("", length(res@.col.name))

    if (verbose)
        message("An R object pointing to ", x,
                " in connection ", conn.id, " is created !")

    .restore.warnings(warnings)

    return (res)
}
