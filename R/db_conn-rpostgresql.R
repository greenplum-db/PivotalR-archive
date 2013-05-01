### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities, using RPostgreSQL to connect

## ------------------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password,
                                    port, madlib)
{
    if (is.null(.localVars$drv$rpostgresql))
        .localVars$drv$rpostgresql <- DBI::dbDriver("PostgreSQL")
    
    n.db <- length(.localVars$db)
    func <- getMethod("dbConnect", signature="PostgreSQLDriver",
                      where=as.environment("package:RPostgreSQL"))
    db.connection <- func(.localVars$drv$rpostgresql,
                         host=host, user=user,
                         dbname=dbname,
                         password=password,
                         port = port)
    if (length(.localVars$conn.id) == 0)
        conn.id <- 1
    else
        conn.id <- max(.localVars$conn.id[,2]) + 1
    
    .localVars$db[[n.db+1]] <- list(
        conn = db.connection,
        conn.id = conn.id,
        host = host,
        user = user,
        dbname = dbname, # database name
        # which R package is used to connected to database
        conn.pkg = "rpostgresql",
        madlib = madlib # madlib schema name
        )

    .localVars$conn.id <- rbind(.localVars$conn.id,
                                   c(conn.id, n.db + 1))
    
    .localVars$conn.type[["rpostgresql"]] <- c(
        .localVars$conn.type[["rpostgresql"]],
        conn.id)
    return(conn.id) # return the connection ID
}

## ------------------------------------------------------------------------

.db.unloadDriver.rpostgresql <- function(drv)
{
    func <- getMethod("dbUnloadDriver", signature="PostgreSQLDriver",
                      where=as.environment("package:RPostgreSQL"))
    func(drv)
}

## ------------------------------------------------------------------------

.db.disconnect.rpostgresql <- function(idx)
{
    func <- getMethod("dbDisconnect", signature="PostgreSQLConnection",
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn)
}

## ------------------------------------------------------------------------

.db.sendQuery.rpostgresql <- function(query, idx)
{
    func <- getMethod("dbSendQuery",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn, query)
}

## ------------------------------------------------------------------------

.db.fetch.rpostgresql <- function(res, n)
{
    func <- getMethod("fetch",
                      signature=c("PostgreSQLResult", "numeric"),
                      where=as.environment("package:RPostgreSQL"))
    func(res, n)
}

## ------------------------------------------------------------------------

.db.getQuery.rpostgresql <- function(query, idx)
{
    func <- getMethod("dbGetQuery",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn, query)
}

## ------------------------------------------------------------------------

.db.listTables.rpostgresql <- function(idx)
{
    func <- getMethod("dbListTables",
                      signature="PostgreSQLConnection",
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn)
}

## ------------------------------------------------------------------------

.db.existsTable.rpostgresql <- function(table, idx)
{
    func <- getMethod("dbExistsTable",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn, table)
}

## ------------------------------------------------------------------------

.db.listFields.rpostgresql <- function(table, idx)
{
    func <- getMethod("dbListFields",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(.localVars$db[[idx]]$conn, table)
}

## ------------------------------------------------------------------------

.db.buildTableDefinition <- function(dbObj, name, obj, field.types,
                                     add.row.names, dist.str, is.temp, ...)
{
    func <- getMethod("dbDataType",
                      signature=c("PostgreSQLObject", "ANY"),
                      where=as.environment("package:RPostgreSQL"))
    if(!is.data.frame(obj))
        obj <- as.data.frame(obj)
    if(!is.null(add.row.names) && add.row.names){
        obj  <- cbind(row.names(obj), obj)  ## can't use row.names= here
        names(obj)[1] <- "row.names"
    }
    if(is.null(field.types)){
        ## the following mapping should be coming from some kind of table
        ## also, need to use converter functions (for dates, etc.)
        field.types <- sapply(obj, func, dbObj = dbObj)
    }

    ## need to create a new (empty) table
    flds <- paste(RPostgreSQL::postgresqlQuoteId(names(field.types)),
                  field.types)
    if (is.temp) tmp.str <- "temp"
    else tmp.str <- ""
    paste("CREATE", tmp.str, "TABLE",
          RPostgreSQL::postgresqlTableRef(name), "\n(",
          paste(flds, collapse=",\n\t"), "\n)", dist.str)
}


## ------------------------------------------------------------------------

.db.writeTable.rpostgresql <- function (table, r.obj, add.row.names, 
                                        overwrite, append, distributed.by,
                                        is.temp, 
                                        idx, header, nrows = 50, sep = ",",
                                        eol="\n", skip = 0, quote = '"',
                                        field.types, ...)
{
    conn <- .localVars$db[[idx]]$conn
    conn.id <- .localVars$db[[idx]]$conn.id
    name <- table
    value <- r.obj

    func1 <- getMethod("dbClearResult",
                       signature="PostgreSQLResult",
                       where=as.environment("package:RPostgreSQL"))
    func2 <- getMethod("dbWriteTable",
                       signature=c("PostgreSQLConnection", "character",
                       "character"),
                       where=as.environment("package:RPostgreSQL"))
    func3 <- getMethod("dbWriteTable",
                       signature=c("PostgreSQLConnection", "character",
                       "data.frame"),
                       where=as.environment("package:RPostgreSQL"))
    
    ## only for GPDB
    ## This why this function is so complicated
    if (is.null(distributed.by)) {
        dist.str <- ""
    } else {
        if (!.is.arg.string(distributed.by))
            stop("distributed.by must be a string or NULL!")
        if (distributed.by == "") # "" means distributed randomly
            dist.str <- "DISTRIBUTED RANDOMLY"
        else
            dist.str <- paste("DISTRIBUTED BY (", distributed.by, ")",
                              sep = "")
    }
    
    if (!append)
    {
        ## need to create the table first
        if (is.character(r.obj)) # create from file
        {
            new.con <- conn
            
            if (is.temp) {
                check.temp <- .db.existsTempTable(name, conn.id)
                name <- check.temp[[2]]
            }
            if((!is.temp && .db.existsTable(name, conn.id)) ||
               (is.temp && check.temp[[1]]))
            {
                if(overwrite)
                {
                    if(!.db.removeTable(name, conn.id))
                    {
                        warning(paste("table", name,
                                      "couldn't be overwritten"))
                        return(FALSE)
                    }
                }
                else if(!append)
                {
                    warning(
                        paste("table", name,
                              "exists in database: aborting dbWriteTable"))
                    return(FALSE)
                }
            }
            
            ## compute full path name (have R expand ~, etc)
            fn <- file.path(dirname(r.obj), basename(r.obj))
            if(missing(header) || missing(add.row.names))
            {
                f <- file(fn, open="r")
                if (skip>0) readLines(f, n=skip)
                txtcon <- textConnection(readLines(f, n=2))
                flds <- count.fields(txtcon, sep)
                close(txtcon)
                close(f)
                nf <- length(unique(flds))
            }
            
            if(missing(header)) header <- nf==2
            
            if(missing(add.row.names))
            {
                if(header)
                    add.row.names <- if(nf==2) TRUE else FALSE
                else
                    add.row.names <- FALSE
            }
            
            new.table <- !.db.existsTable(name, conn.id)
            if(new.table)
            {
                ## need to init table, say, with the first nrows lines
                d <- read.table(fn, sep=sep, header=header, skip=skip,
                                nrows=nrows, ...)
                if (missing(field.types)) field.types <- NULL
                sql <- .db.buildTableDefinition(new.con, table, d,
                                                field.types, add.row.names,
                                                dist.str, is.temp)
                rs <- try(.db.sendQuery(sql, conn.id))
                if(is(rs, RPostgreSQL:::ErrorClass)){
                    warning("could not create table: aborting postgresqlImportFile")
                    return(FALSE)
                }
                else
                    func1(rs)
            }
            else if(!append)
            {
                warning(sprintf(
                    "table %s already exists -- use append=TRUE?", name))
            }

            ## After the table has been created, one can append data to it
            func2(conn = .localVars$db[[idx]]$conn,
                  name = name, value = value,
                  overwrite = overwrite, append = TRUE,
                  header = header, nrows = nrows,
                  sep = sep,
                  eol=eol, skip = skip, quote = quote,
                  field.types = field.types, ...)
        }
        else # create table from a data frame --------------------------------
        {
            if (is.temp) check.temp <- .db.existsTempTable(table, conn.id)
            if((!is.temp && .db.existsTable(name, conn.id)) ||
               (is.temp && check.temp[[1]]))
            {
                if (overwrite) {
                    if (!.db.removeTable(name, conn.id))
                    {
                        warning(paste("table", name,
                                      "couldn't be overwritten"))
                        return(FALSE)
                    }
                }
                else if (!append)
                {
                    warning(
                        paste("table", name,
                              "exists in database: aborting assignTable"))
                    return(FALSE)
                }         
            }
            else
            {
                if (missing(field.types)) field.types <- NULL
                sql <- .db.buildTableDefinition(conn, table, r.obj,
                                                field.types, add.row.names,
                                                dist.str, is.temp)
                rs <- try(.db.sendQuery(sql, conn.id))
                if (is.temp) name <- (.db.existsTempTable(table,
                                                          conn.id))[[2]]
                if(is(rs, RPostgreSQL:::ErrorClass))
                {
                    warning("could not create table: aborting assignTable")
                    return(FALSE)
                }
                else
                {
                    func1(rs)#######
                }
            }

            ## After the table has been created, one can append data to it
            func3(conn = .localVars$db[[idx]]$conn,
                  name = name, value = value,
                  row.names = add.row.names,
                  overwrite = overwrite, append = TRUE)
        }
    }
}

## ------------------------------------------------------------------------

.db.readTable.rpostgresql <- function (table, row.names, idx)
{
    func <- getMethod("dbReadTable",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(conn = .localVars$db[[idx]]$conn,
         name = table, row.names = row.names)
}

## ------------------------------------------------------------------------

.db.removeTable.rpostgresql <- function (table, idx)
{
    func <- getMethod("dbRemoveTable",
                      signature=c("PostgreSQLConnection", "character"),
                      where=as.environment("package:RPostgreSQL"))
    func(conn = .localVars$db[[idx]]$conn,
         name = table)
}
