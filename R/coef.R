coef.logregr.madlib <- coef.lm.madlib <- function(object, ...)
{
    if (object$has.intercept)
        nams <- c("(Intercept)", object$ind.vars)
    else
        nams <- object$ind.vars
    nams <- .reverse.consistent.func(nams)
    for (i in seq_len(length(object$col.name)))
        if (object$col.name[i] != object$appear[i])
            nams <- gsub(object$col.name[i], object$appear[i], nams)
    setNames(object$coef, nams)
}


coef.logregr.madlib.grps <- coef.lm.madlib.grps <- function(object, ...)
lapply(object, coef)

