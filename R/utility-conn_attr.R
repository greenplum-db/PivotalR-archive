
## ------------------------------------------------------------------------
## Small utility functions to extract information
## about a given connection
## ------------------------------------------------------------------------

dbname <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$dbname
}

## ------------------------------------------------------------------------

host <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$host
}

## ------------------------------------------------------------------------

user <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$user
}

## ------------------------------------------------------------------------

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

## ------------------------------------------------------------------------

## connection itself
conn <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$conn
}

## ------------------------------------------------------------------------

## Get the DBMS version info
dbms <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    res <- .db.getQuery("select version()", conn.id)
    as.character(res)
}

## ------------------------------------------------------------------------

schema.madlib <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    id <- .localVars$conn.id[.localVars$conn.id[,1] == conn.id, 2]
    .localVars$db[[id]]$madlib
}

## ------------------------------------------------------------------------

madlib.version <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    as.character(.db.getQuery(paste("select ", schema.madlib(conn.id),
                                    ".version()", sep = ""),
                              conn.id))
}

madlib <- function (conn.id = 1)
{
    madlib.version(conn.id)
}

## ------------------------------------------------------------------------

## Are the two connections equivalent? 
conn.eql <- function (conn.id1, conn.id2)
{
    if (!.is.conn.id.valid(conn.id1) || !.is.conn.id.valid(conn.id2))
        stop("At least one connection does not exist!")

    ## I do not think the users or conection packages
    ## have to be the same.
    if (dbname(conn.id1) == dbname(conn.id2) &&
        host(conn.id1) == host(conn.id2) &&
        dbms(conn.id1) == dbms(conn.id2))
        TRUE
    else
        FALSE
}
