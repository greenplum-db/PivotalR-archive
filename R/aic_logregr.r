extractAIC.logregr.madlib <- function(fit, scale=0, k=2, ...)
{
    edf <- length(fit$coef)
    c(edf, -2 * fit$log_likelihood + 2 * edf)
}

logLik.logregr.madlib <- function(object, ...)
{
    ll <- object$log_likelihood
    attr(ll, "df") <- length(object$coef)
    class(ll) <- "logLik"
    ll
}

extractAIC.logregr.madlib.grps <- function(fit, scale=0, k=2, ...)
lapply(extractAIC, fit, scale=scale, k=2)

logLik.logregr.madlib.grps <- function(object, ...)
lapply(logLik, object)

AIC.logregr.madlib.grps <- function(object, ..., k=2)
sapply(AIC, object, k=k)

