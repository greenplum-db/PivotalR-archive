extractAIC.glm.madlib <- extractAIC.logregr.madlib <- function(fit, scale=0, k=2, ...)
{
    edf <- length(fit$coef)
    c(edf, -2 * fit$log_likelihood + k * edf)
}

## ----------------------------------------------------------------------

logLik.glm.madlib <- logLik.logregr.madlib <- function(object, ...)
{
    ll <- object$log_likelihood
    attr(ll, "df") <- length(object$coef)
    class(ll) <- "logLik"
    ll
}

## ----------------------------------------------------------------------

extractAIC.glm.madlib.grps <- extractAIC.logregr.madlib.grps <- function(fit, scale=0, k=2, ...)
    lapply(fit, extractAIC, scale=scale, k=k, ...)

## ----------------------------------------------------------------------

logLik.glm.madlib.grps <- logLik.logregr.madlib.grps <- function(object, ...)
    lapply(object, logLik, ...)

## ----------------------------------------------------------------------

AIC.glm.madlib.grps <- AIC.logregr.madlib.grps <- function(object, ..., k=2)
    sapply(object, AIC, k=k, ...)

