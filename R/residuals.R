## ----------------------------------------------------------------------
## Extract residual info
## ----------------------------------------------------------------------

residuals.lm.madlib <- function(object, ...)
{
    with(object$data, object$terms[[2]]) - predict(object, object$data)
}

## ----------------------------------------------------------------------

residuals.logregr.madlib <- function(object, ...)
{
    with(object$data, object$terms[[2]]) - predict(object, object$data)
}

## ----------------------------------------------------------------------

residuals.lm.madlib.grps <- function(object, ...)
    lapply(residuals, object)

## ----------------------------------------------------------------------

residuals.logregr.madlib.grps <- function(object, ...)
    lapply(residuals, object)
