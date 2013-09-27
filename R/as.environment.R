as.environment.db.table <- as.environment.db.view <- function(x, ...)
{
    lst <- sapply(names(x), function(i) x[[i]], simplify=FALSE)
    as.environment(lst)
}
