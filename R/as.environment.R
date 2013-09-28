as.environment.db.table <- as.environment.db.view <- function(x, ...)
{
    lst <- sapply(names(x), function(i) x[[i]], simplify=FALSE)
    list2env(lst)
}

## ----------------------------------------------------------------------

with.db.table <- with.db.view <- function(data, expr, ...)
eval(substitute(expr), as.environment(data), enclos=parent.frame())
