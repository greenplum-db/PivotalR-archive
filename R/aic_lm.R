extractAIC.lm.madlib <- function(fit, scale=0, k=2, ...)
{
    edf <- length(fit$coef)
    sse <- lookat(sum(resid(fit)^2))
    c(edf, fit$nobs * log(sse/fit$nobs) + k * edf)
}

## ----------------------------------------------------------------------

logLik.lm.madlib <- function(object, ...)
{
    sse <- lookat(sum(residuals(object)^2))
    n <- object$nobs
    ll <- 0.5 * (-n * (log(2 * pi) + 1 - log(n) + log(sse)))
    attr(ll, "df") <- length(object$coef) + 1
    class(ll) <- "logLik"
    ll
}

## ----------------------------------------------------------------------

extractAIC.lm.madlib.grps <- function(fit, scale=0, k=2, ...)
    lapply(fit, extractAIC, scale=scale, k=k, ...)

## ----------------------------------------------------------------------

logLik.lm.madlib.grps <- function(object, ...)
    lapply(object, logLik, ...)

## ----------------------------------------------------------------------

AIC.lm.madlib.grps <- function(object, ..., k=2)
    sapply(object, AIC, k=k, ...)

