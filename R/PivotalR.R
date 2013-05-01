
## ------------------------------------------------------------------------
## start shiny GUI
## ------------------------------------------------------------------------

PivotalR <- function ()
{
    ## PivotalR's installation path
    .localVars$pkg.path <- path.package(.this.pkg.name)
    ## gui folder installation path
    gui.dir <- paste(.localVars$pkg.path, "/gui", sep = "")

    message("\nWelcome to poor men's Alpine. It is free ...\n")
    message("Press Ctrl + c to stop.")
    runApp(gui.dir)
}
