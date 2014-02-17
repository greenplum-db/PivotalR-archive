## Execute on unloading

## release local variables

.onUnload <- function(libpath)
{
    ## close all unclosed database connections
    for (i in .localVars$conn.id[,1])
        db.disconnect(conn.id = i, verbose = FALSE, force = TRUE)

    ## also unload all db connection drivers
    pkg.names <- names(.localVars$drv)
    for (pkg in pkg.names)
        .db.unloadDriver(pkg)

    ## turn off the `special cbind()' :
    ## source(paste(.localVars$pkg.path, "/auto/disable.cbind2.R", sep = ""))
    ## eval(parse(text = "methods:::bind_activation(on = FALSE)"))
}
