## Execute on attach

## define local variables

.onAttach <- function(libname, pkgname)
{
    ## manage database connections
    .localVars$db <- list()

    ## store the connection ID
    ## A n x 2 matrix, first column is conn.id,
    ## second is the index for db list
    ## It maps conn.id to the connection object stored
    ## in .localVars$db
    .localVars$conn.id <- integer(0) 
    
    ## R connection package
    ## right now, only support RPostgreSQL
    ## set up records for which connection ID belongs to which connection type
    ## source("supported-connections.R", local = TRUE)
    .localVars$conn.type <- list()
    for (i in seq(along=.supported.connections))
        .localVars$conn.type[[tolower(.supported.connections[i])]] <- integer(0)
}
