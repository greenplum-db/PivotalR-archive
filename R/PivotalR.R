
## -----------------------------------------------------------------------
## start shiny GUI
## -----------------------------------------------------------------------

##' This function launches a shiny server which provides a graphical
##' interface for PivotalR. Press Ctrl+c to stop the shiny server.
##'
##' The graphical interface for PivotalR is very easy to use. Just follow
##' the instructions on screen. The GUI is still at a very early stage
##' and has only very limited functionality. We will add more
##' functionalities into the GUI in the future versions.
##'
##' @name GUI
##' @aliases PivotalR pivotalr
##' @title Graphical interface for PivotalR based upon shiny
##'
##' @references
##' [1] RStudio and Inc. (2013). shiny: Web Application
##' Framework for R. R package version 0.6.0.
##' \url{http://CRAN.R-project.org/package=shiny}
##'
##' [2] shiny website, \url{http://www.rstudio.com/shiny/}
##'
##' @keywords IO utilities
##'
##' @rdname pivotalr
##' @export
##'
PivotalR <- function ()
{
    if (length(.localVars$db) == 0)
        stop("No database connections! ",
             "You need at least one database connection before launching",
             " the graphical interface!")

    if (!("shiny" %in% .get.installed.pkgs())) {
        message(paste("Package shiny",
                      " is going to be installed so that ",
                      .this.pkg.name,
                      " could run its own shiny app.\n\n", sep = ""))
        install.packages(pkgs = "shiny")
        if (!("shiny" %in% .get.installed.pkgs()))
            stop("The package shiny could not be installed!")
    }

    library(shiny)

    ## PivotalR's installation path
    ## .localVars$pkg.path <- path.package(.this.pkg.name)
    ## gui folder installation path
    gui.dir <- paste(.localVars$pkg.path, "/gui", sep = "")

    message("\nWelcome to PivotalR GUI. It is free and a beta version ...\n")
    message("Press Ctrl + c to stop.")
    shiny::runApp(gui.dir)
}

## -----------------------------------------------------------------------

##' @rdname pivotalr
##' @export
##'
pivotalr <- function ()
{
    PivotalR()
}

## ------------------------------------------------------------------------

##' This function prints nothing and is used only in GUI.
##'
##' @name print.none.obj
##' @title Function used in GUI to print absolutely nothing
##'
##' @param x A \code{none.obj} object. The content of this object does not
##' matter. It is used to return a value which makes the GUI print
##' nothing on the screen.
##'
##' @param ... Not used
##'
##' @method print none.obj
##'
##' @seealso \code{\link{PivotalR}} launches the GUI for PivotalR.
##'
##' @keywords IO print utilities
##'
##' @rdname print.non.obj
##' @export
##'
print.none.obj <- function (x, ...)
{
    cat("")
}
