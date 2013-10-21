
## -----------------------------------------------------------------------
## start shiny GUI
## -----------------------------------------------------------------------

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

pivotalr <- function ()
{
    PivotalR()
}

## -----------------------------------------------------------------------

print.none.obj <- function (x, ...) { cat("") }
