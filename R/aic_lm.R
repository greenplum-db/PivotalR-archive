extractAIC.lm.madlib <- function(fit, scale=0, k=2, ...)
{
    edf <- length(fit$coef)
    c(edf, nobs(fit) * log(fit$sse/nobs(fit)) + k * edf)
}

## ----------------------------------------------------------------------

logLik.lm.madlib <- function(object, ...)
{
    ll <- nobs(object) * log(object$sse/nobs(object))
    attr(ll, "df") <- length(object$coef)
    class(ll) <- "logLik"
    ll
}

## ----------------------------------------------------------------------

extractAIC.lm.madlib.grps <- function(fit, scale=0, k=2, ...)
lapply(extractAIC, fit, scale=scale, k=2)

## ----------------------------------------------------------------------

logLik.lm.madlib.grps <- function(object, ...)
lapply(logLik, object)

## ----------------------------------------------------------------------

AIC.lm.madlib.grps <- function(object, ..., k=2)
sapply(AIC, object, k=k)

