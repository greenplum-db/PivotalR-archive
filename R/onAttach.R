## Execute on attach

## -----------------------------------------------------------------------

## The environment that contains all testing environment variables
.testing.env <- new.env(parent = getNamespace(.this.pkg.name))
.continuous.env <- new.env(parent = globalenv()) # used for continuous testing

## -----------------------------------------------------------------------

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

    packageStartupMessage("\nTo launch the graphical user interface, run the function pivotalr() !")

    ## turn on cbind implementation using cbind2
    .localVars$R.ver <- gsub("^R version (\\d+\\.\\d+).*$", "\\1", R.version.string)
    .localVars$pkg.path <- .get.package.path()
    ## source(paste(.localVars$pkg.path, "/auto/enable.cbind2.R", sep = ""))
    ## eval(parse(text = "methods:::bind_activation(on = TRUE)"))
}
