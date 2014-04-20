## ----------------------------------------------------------------------
## Universal database connection utilities
## Multiple R connection packages are supported

## Note: Internal functions do not need argument check
## only functions that are exposed to the users need the check
## ----------------------------------------------------------------------

## connect to a database using a specific R package
## Right now, only RPostgreSQL is supported
## If the connection package is not installed, it will
## be automatically installed
## A driver will be automatically created for connection package
db.connect <- function (host = "localhost", user = Sys.getenv("USER"), dbname = user,
                        password = "", port = 5432,
                        madlib = "madlib", conn.pkg = "RPostgreSQL",
                        default.schemas = NULL, verbose = TRUE, quick = FALSE)
{
    if (quick) {
        eval(parse(text = paste("suppressMessages(library(", conn.pkg, "))")))
        command <- paste(".db.connect.", tolower(conn.pkg), "(host=\"", host,
                         "\", user=\"", user, "\", dbname=\"", dbname,
                         "\", password=\"", password, "\", port=", port,
                         ", madlib=\"", madlib, "\"",
                         ")", sep = "")
        result <- eval(parse(text = command))
        .madlib.version.number(result) # record the madlib version number

        if (!is.null(default.schemas)) {
            res <- .db.getQuery(paste("set search_path =",
                                      default.schemas), conn.id = result)
            if (is(res, .err.class))
                stop("Could not set the default schemas ! ",
                     "default.schemas must be a set of schema names ",
                     "separated by commas. One can also use the ",
                     "function db.default.schemas or db.search.path ",
                     "to display or set the current default schemas.")
        }

        res <- db.q(paste("set application_name = '",
                          .this.pkg.name, "'", sep = ""),
                    conn.id = result, verbose = FALSE)

        return (result)
    }

    ## argument type check
    if (!.is.arg.string(host) ||
        !.is.arg.string(user) ||
        !.is.arg.string(dbname) ||
        !.is.arg.string(password) ||
        !.is.arg.string(conn.pkg))
        stop("Host, user, dbname, password (could be an empty string) and the connection package should all be strings!")

    ## use one of the R connection package to connect to database
    conn.pkg.name <- tolower(conn.pkg)
    if (conn.pkg.name %in% tolower(.supported.connections)) # make sure that the package is supported
    {
        i <- which(tolower(.supported.connections) == conn.pkg.name)
        pkg.to.load <- .supported.connections[i]
        ## if the package is not installed, install it
        installed.pkgs <- .get.installed.pkgs()
        if (!(conn.pkg.name %in% installed.pkgs))
        {
            if (conn.pkg.name == "rpostgresql" && !("dbi" %in% installed.pkgs)) {
                if (verbose) message("Package DBI is going to be installed!")
                install.packages(paste(.localVars$pkg.path, "/dbi/DBI.tar.gz",
                                       sep = ""), repos = NULL, type = "source")
            }
            if (verbose)
                message(paste("Package ", pkg.to.load,
                              " is going to be installed so that ",
                              .this.pkg.name,
                              " could connect to databases.\n", sep = ""))
            install.packages(pkgs = pkg.to.load)
            if (!(conn.pkg.name %in% .get.installed.pkgs()))
                stop("The package could not be installed!")
        }

        if (verbose)
            eval(
                parse(
                    text = paste(
                    "library(", pkg.to.load, ")", sep = "")))
        else
            eval(
                parse(
                    text = paste(
                    "suppressMessages(library(", pkg.to.load, "))", sep = "")))

        command <- paste(".db.connect.", conn.pkg.name, "(host=\"", host,
                         "\", user=\"", user, "\", dbname=\"", dbname,
                         "\", password=\"", password, "\", port=", port,
                         ", madlib=\"", madlib, "\"",
                         ")", sep = "")
        result <- eval(parse(text = command))
        if (verbose)
            cat(paste("Created a connection to database with ID",
                      result, "\n"))
        .madlib.version.number(result) # record the madlib version number

        if (!is.null(default.schemas)) {
            res <- .db.getQuery(paste("set search_path =",
                                      default.schemas), conn.id = result)
            if (is(res, .err.class))
                stop("Could not set the default schemas ! ",
                     "default.schemas must be a set of schema names ",
                     "separated by commas. One can also use the ",
                     "function db.default.schemas or db.search.path ",
                     "to display or set the current default schemas.")
        }

        res <- db.q(paste("set application_name = '",
                          .this.pkg.name, "'", sep = ""),
                    conn.id = result, verbose = FALSE)

        ## search_path = db.default.schemas(conn.id = result)[1,]
        tryCatch(
        {
            tbl <- .unique.string()
            db.q("
                create table", tbl, "(idx integer);
                drop table if exists", tbl, conn.id = result,
                 verbose = FALSE)
        },
            error = function(s) {
                warning("There is no default schema or the default schema is not writable! ",
                        "Some functions need to create intermediate tables. ",
                        "Use db.default.schemas to set a proper default scehma.\n")
            })

        return (result)
    }
    else
    {
        stop("Right now, only ", .supported.connections,
             " is supported to connected to database.\n")
    }
}

## ----------------------------------------------------------------------

## show/set the current search path
db.default.schemas <- function (conn.id = 1, set = NULL)
{
    if (!.is.conn.id.valid(conn.id))
        stop(conn.id, " is not a valid connection ID !")

    if (is.null(set)) {
        res <- .db.getQuery("show search_path", conn.id = conn.id)
        if (is(res, .err.class))
            stop("Could not show the default schemas ! ")
        res
    } else {
        res <- .db.getQuery(paste("set search_path =",
                                  set), conn.id = conn.id)
        if (is(res, .err.class))
            stop("Could not set the default schemas ! ",
                 "default.schemas must be a set of schema names ",
                 "separated by commas.")

        tryCatch(
        {
            tbl <- .unique.string()
            db.q("
                create table", tbl, "(idx integer);
                drop table if exists", tbl, conn.id = conn.id,
                 verbose = FALSE)
        },
            error = function(s) {
                warning("There is no default schema or the default schema is not writable! ",
                        "Some functions need to create intermediate tables. ",
                        "Use db.default.schemas to set a proper default scehma.\n")
            })
    }
}

db.search.path <- function (conn.id = 1, set = NULL)
{
    db.default.schemas(conn.id, set)
}

## ----------------------------------------------------------------------

## disconnect a connection
db.disconnect <- function (conn.id = 1, verbose = TRUE, force = FALSE)
{
    ## check whether this connection exists
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")

    idx <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    conn.pkg <- .localVars$db[[idx]]$conn.pkg
    command <- paste(".db.disconnect.", conn.pkg, "(idx=", idx, ")",
                     sep = "")
    res <- eval(parse(text = command))
    if (res || force)
    {
        .localVars$db[[idx]] <- NULL
        .localVars$conn.type[[conn.pkg]] <- .localVars$conn.type[[conn.pkg]][-which(.localVars$conn.type[[conn.pkg]]==conn.id)]
        .localVars$conn.id <- .localVars$conn.id[.localVars$conn.id[,1] != conn.id,] # delete the conn.id from the array
        if (length(.localVars$db) == 1) .localVars$conn.id <- matrix(.localVars$conn.id, nrow = 1)

        ## update conn.id mapping info
        for (i in seq_len(length(.localVars$db)))
        {
            id <- .localVars$db[[i]]$conn.id
            .localVars$conn.id[.localVars$conn.id[,1] == id, 2] <- i
        }

        if (verbose) {
            if (force)
                cat(paste("Connection", conn.id, "is forcely removed!\n"))
            else
                cat(paste("Connection", conn.id, "is disconnected!\n"))
        }
    }
    else
    {
        cat("There was a problem and the connection cannot be disconnected")
    }

    return (res)
}

## ----------------------------------------------------------------------

.get.dbms.str <- function (conn.id)
{
    dbms.str <- dbms(conn.id = conn.id)
    if (gsub(".*(HAWQ).*", "\\1", dbms.str, perl=T) == "HAWQ") {
        db.str <- "HAWQ"
        version.str <- gsub(".*HAWQ[^\\d]+?([\\d\\.]+?).*", "\\1",
                            dbms.str, perl=T)
    } else if (gsub(".*(Greenplum).*", "\\1", dbms.str,
                  perl=T) == "Greenplum") {
        db.str <- "Greenplum"
        version.str <- gsub(".*Greenplum[^\\d]+?([\\d\\.]+?).*",
                            "\\1", dbms.str, perl=T)
    } else {
        db.str <- "PostgreSQL"
        version.str <- gsub(".*PostgreSQL[^\\d]+?([\\d\\.]+?).*",
                            "\\1", dbms.str, perl=T)
    }
    list(db.str = db.str, version.str = version.str)
}

## ----------------------------------------------------------------------

## List all connection info
db.list <- function ()
{
    n.conn <- length(.localVars$db)
    cat("\nDatabase Connection Info\n")
    if (n.conn == 0)
    {
        cat("\n## -------------------------------\n")
        cat("******** No database connections! ********\n\n")
    }
    else
    {
        for (i in seq_len(dim(.localVars$conn.id)[1]))
        {
            idx <- .localVars$conn.id[i,]
            cat("\n## -------------------------------\n")
            cat(paste("[Connection ID ", idx[1], "]\n", sep = ""))
            cat(paste("Host     :    ", .localVars$db[[idx[2]]]$host,
                      "\n", sep = ""))
            cat(paste("User     :    ", .localVars$db[[idx[2]]]$user,
                      "\n", sep = ""))
            cat(paste("Database :    ", .localVars$db[[idx[2]]]$dbname,
                      "\n", sep = ""))

            db <- .get.dbms.str(idx[1])
            cat("DBMS     :   ", db$db.str, db$version.str, "\n")

            if (identical(.localVars$db[[idx[2]]]$madlib.v, numeric(0)))
                cat("MADlib   :    not installed in schema", schema.madlib(idx[1]), "\n")
            else
                cat("MADlib   :    installed in schema", schema.madlib(idx[1]), "\n")

            ## pkg <- .localVars$db[[idx[2]]]$conn.pkg
            ## id <- which(tolower(.supported.connections) == pkg)
            ## cat(paste("Conn pkg :    ", .supported.connections[id],
            ##           "\n", sep = ""))
        }
        cat("\n")
    }
}

## ----------------------------------------------------------------------

## list tables and views in the connection
db.objects <- function (search = NULL, conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("Connection ID ", conn.id, " is not valid!")

    res <- .db.getQuery("select table_schema, table_name from information_schema.tables", conn.id = conn.id)

    if (is.null(search)) {
        res <- paste(res[,1], res[,2], sep = ".")
        return (res[order(res)])
    }

    search <- gsub("\\.", "\\\\.", search)
    final.res <- character(0)
    for (i in seq_len(dim(res)[1])) {
        name <- paste(res[i,1], ".", res[i,2], sep = "")
        find <- gsub(search, "", name, perl = TRUE)
        if (find != name)
            final.res <- rbind(final.res, res[i,])
    }
    if (!identical(final.res, character(0))) {
        res <- paste(final.res[,1], final.res[,2], sep = ".")
        return (res[order(res)])
    } else
        NULL
}

## ----------------------------------------------------------------------

## does an object exist?
db.existsObject <- function (name, conn.id = 1, is.temp = FALSE)
{
    warns <- .suppress.warnings (conn.id)
    if (length(name) == 1) name <- strsplit(name, "\\.")[[1]]
    if (length(name) != 1 && length(name) != 2)
        stop("The formation of object name is wrong!")
    if (length(name) == 2) {
        if (is.temp) stop("Temporary tables may not specify a schema name!")
        schema <- name[1]
        table <- name[2]
        ct <- db.q(paste("select count(*) from ",
                         "information_schema.tables where ",
                         "table_name = '",
                         .strip(table, "\""),
                         "' and table_schema = '",
                         .strip(schema, "\""), "'", sep = ""),
                   conn.id=conn.id, verbose = FALSE)
        if (ct == 0) {
            .restore.warnings(warns)
            FALSE
        } else {
            .restore.warnings(warns)
            TRUE
        }
    } else {
        if (is.temp) {
            .restore.warnings(warns)
            .db.existsTempTable(name, conn.id)
        } else {
            schemas <- arraydb.to.arrayr(
                db.q("select current_schemas(True)",
                     conn.id=conn.id, verbose = FALSE),
                type = "character")
            table_schema <- character(0)
            for (schema in schemas)
                if (.db.existsTable(c(schema, name), conn.id))
                    table_schema <- c(table_schema, schema)
            if (identical(table_schema, character(0))) {
                .restore.warnings(warns)
                FALSE
            } else {
                .restore.warnings(warns)
                TRUE
            }
        }
    }
}

## ----------------------------------------------------------------------
## All the following function are used inside the package only
## ----------------------------------------------------------------------

## ----------------------------------------------------------------------

## fetch the result of sendQuery
.db.fetch <- function (res, n = 500)
{
    idx <- .localVars$conn.id[.localVars$conn.id[,1] == res$conn.id, 2]
    command <- paste(".db.fetch.", .localVars$db[[idx]]$conn.pkg,
                     "(res = res$res, n = n)", sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

## unload driver for a specific connection package
.db.unloadDriver <- function (pkg)
{
    command <- paste(".db.unloadDriver.", pkg, "(drv=",
                     .localVars$drv[[pkg]], ")", sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

.db.sendQuery <- function (query, conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.sendQuery.", .localVars$db[[id]]$conn.pkg,
                     "(query=query, idx=id)", sep = "")
    list(res = eval(parse(text = command)), conn.id = conn.id)
}

## ----------------------------------------------------------------------

.db.clearResult <- function(res)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == res[[2]], 2]
    command <- paste(".db.clearResult.", .localVars$db[[id]]$conn.pkg,
                     "(res$res)", sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

.db.getQuery <- function (query, conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.getQuery.", .localVars$db[[id]]$conn.pkg,
                     "(query=query, idx=id)", sep = "")
    eval(parse(text = command))
}

## -----------------------------------------------------------------------

.db.listTables <- function (conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.listTables.", .localVars$db[[id]]$conn.pkg,
                     "(idx=id)", sep = "")
    eval(parse(text = command))
}

## -----------------------------------------------------------------------

.db.existsTable <- function (table, conn.id = 1)
{
    ## id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    ## command <- paste(".db.existsTable.", .localVars$db[[id]]$conn.pkg,
    ##                  "(table=table, idx=id)", sep = "")
    ## eval(parse(text = command))
    if (length(table) == 1) {
        schema.str <- try(.db.table.schema.str(table, conn.id), silent = TRUE)
        if (is(schema.str, .err.class)) return (FALSE)
        #tbl.name <- table
    } else {
        schema.str <- paste("table_name = '", .strip(table[2], "\""),
                            "' and table_schema = '", .strip(table[1], "\""), "'", sep = "")
        #tbl.name <- table[2]
    }

    ct <- db.q("select count(*) from information_schema.tables where ",
        .strip(schema.str, "\""), sep = "", conn.id=conn.id,
        verbose = FALSE)
    if (ct == 0)
        FALSE
    else
        TRUE
}

## ----------------------------------------------------------------------

.db.existsTempTable <- function (table, conn.id = 1)
{
    if (length(table) == 2)
    {
        schema.str <- strsplit(table[1], "_")[[1]]
        if (schema.str[1] != "pg" || schema.str[2] != "temp")
            return (list(FALSE, table))
        else
            return (list(TRUE, table))
    }
    else
    {
        schemas <- arraydb.to.arrayr(
            .db.getQuery("select current_schemas(True)", conn.id),
            type = "character")
        table_schema <- character(0)
        for (schema in schemas)
            if (.db.existsTable(c(schema, table), conn.id))
                table_schema <- c(table_schema, schema)

        if (identical(table_schema, character(0))) return (list(FALSE, c("", table)))
        schema.str <- strsplit(table_schema, "_")
        for (i in seq_len(length(schema.str))) {
            str <- schema.str[[i]]
            if (str[1] == "pg" && str[2] == "temp")
                return (list(TRUE, c(table_schema[i], table)))
        }
        return (list(FALSE, c("", table)))
    }
}

## ----------------------------------------------------------------------

.db.listFields <- function (table, conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.listFields.", .localVars$db[[id]]$conn.pkg,
                     "(table=table, idx=id)", sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

.db.writeTable <- function (table, r.obj, add.row.names = TRUE,
                            overwrite = FALSE, append = FALSE,
                            distributed.by = NULL, # only for GPDB
                            is.temp = FALSE,
                            conn.id = 1, header = FALSE, nrows = 50,
                            sep = ",",
                            eol="\n", skip = 0, quote = "\"", ...)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.writeTable.", .localVars$db[[id]]$conn.pkg,
                     "(table=table, r.obj=r.obj, add.row.names=add.row.names,
                      overwrite=overwrite, append=append,
                      distributed.by=distributed.by,
                      is.temp=is.temp, idx=id,
                      header=header, nrows=nrows, sep=sep, eol=eol,
                      skip=skip, quote=quote, ...)",
                     sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

.db.readTable <- function (table, rown.names = "row.names", conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.readTable.", .localVars$db[[id]]$conn.pkg,
                     "(table=table, row.names=row.names, idx=id)",
                     sep = "")
    eval(parse(text = command))
}

## ----------------------------------------------------------------------

.db.removeTable <- function(table, conn.id = 1)
{
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    command <- paste(".db.removeTable.", .localVars$db[[id]]$conn.pkg,
                     "(table=table, idx=id)", sep = "")
    eval(parse(text = command))
}
