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

