as.environment.db.obj <- function(x, ...)
{
    lst <- sapply(names(x), function(i) x[[i]], simplify=FALSE)
    list2env(lst)
}

## ----------------------------------------------------------------------

with.db.obj <- function(data, expr, ...)
    eval(substitute(expr), as.environment(data), enclos=parent.frame())
