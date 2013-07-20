
## ------------------------------------------------------------------------
## start shiny GUI
## ------------------------------------------------------------------------

PivotalR <- function ()
{
    
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
    .localVars$pkg.path <- path.package(.this.pkg.name)
    ## gui folder installation path
    gui.dir <- paste(.localVars$pkg.path, "/gui", sep = "")

    message("\nWelcome to PivotalR GUI. It is free ...\n")
    message("Press Ctrl + c to stop.")
    runApp(gui.dir)
}

## ------------------------------------------------------------------------

pivotalr <- function ()
{
    PivotalR()
}
