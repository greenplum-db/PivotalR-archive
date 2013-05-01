## Execute on unloading

## release local variables

.onUnload <- function(libpath)
{
    ## close all unclosed database connections
    for (i in seq(along=.localVars$db))
        db.disconnect(conn.id = i, verbose = FALSE)

    ## also unload all db connection drivers
    pkg.names <- names(.localVars$drv)
    for (pkg in pkg.names)
        .db.unloadDriver(pkg)
}
