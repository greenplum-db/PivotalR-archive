
## -----------------------------------------------------------------------
## Small utility functions to extract information
## about a given connection
## -----------------------------------------------------------------------

dbname <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$dbname
}

## -----------------------------------------------------------------------

host <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$host
}

## -----------------------------------------------------------------------

user <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$user
}

## -----------------------------------------------------------------------

## connection package
conn.pkg <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    pkg <- .localVars$db[[id]]$conn.pkg
    i <- which(tolower(.supported.connections) == pkg)
    .supported.connections[i]
}

## -----------------------------------------------------------------------

## connection port
port <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$port
}

## -----------------------------------------------------------------------

## connection itself
conn <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$conn
}

## -----------------------------------------------------------------------

## Get the DBMS version info
dbms <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    res <- .db.getQuery("select version()", conn.id)
    as.character(res)
}

## -----------------------------------------------------------------------

schema.madlib <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$madlib
}

## -----------------------------------------------------------------------

madlib.version <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    exists <- .db.getQuery(paste("select count(*) from ",
                                 "information_schema.routines where ",
                                 "routine_name = 'version' and ",
                                 "routine_schema = '", schema.madlib(conn.id),
                                 "'", sep = ""),
                           conn.id)[1,1]
    if (exists == 1)
        res <- try(.db.getQuery(paste("select ", schema.madlib(conn.id),
                                      ".version()", sep = ""),
                                conn.id), silent = TRUE)
    if (exists == 0 || is(res, .err.class)) {
        message("\nWarning: Madlib does not exist in database ", dbname(conn.id),
                " schema ", schema.madlib(conn.id), ".")
        message("So all functions starting with 'madlib.' will not work.")
        message("But you can still use other functions with just a few exceptions.")
        return (character(0))
    }
    return (as.character(res))
}

## -----------------------------------------------------------------------

madlib <- function (conn.id = 1)
{
    madlib.version(conn.id)
}

## -----------------------------------------------------------------------

## extract the version numbers
.madlib.version.number <- function (conn.id = 1)
{
    version <- madlib.version(conn.id)
    idx <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    if (identical(version, character(0))) {
        .localVars$db[[idx]]$madlib.v <- numeric(0)
        return (numeric(0))
    } else {
        num <- gsub("MADlib version: (\\d+\\.\\d+.\\d+).*", "\\1",
                    version, perl=T)
        .localVars$db[[idx]]$madlib.v <- num
        num
    }
}

## -----------------------------------------------------------------------

## Are the two connections equivalent?
conn.eql <- function (conn.id1, conn.id2)
{
    if (!.is.conn.id.valid(conn.id1) || !.is.conn.id.valid(conn.id2))
        stop("At least one connection does not exist!")

    ## I do not think the users or conection packages
    ## have to be the same.
    if (dbname(conn.id1) == dbname(conn.id2) &&
        host(conn.id1) == host(conn.id2) &&
        dbms(conn.id1) == dbms(conn.id2) &&
        port(conn.id1) == port(conn.id2))
        TRUE
    else
        FALSE
}
