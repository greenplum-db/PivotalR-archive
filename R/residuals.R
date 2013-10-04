## ----------------------------------------------------------------------
## Extract residual info
## ----------------------------------------------------------------------

.compute.resid <- function(object)
{
    (eval(parse(text=paste0("with(object$data,",
               deparse(object$terms[[2]]), ")")))
     - predict(object, object$data))
}

residuals.lm.madlib <- function(object, ...)
    .compute.resid(object)

## ----------------------------------------------------------------------

residuals.logregr.madlib <- function(object, ...)
    .compute.resid(object)

## ----------------------------------------------------------------------

residuals.lm.madlib.grps <- function(object, ...)
    lapply(residuals, object)

## ----------------------------------------------------------------------

residuals.logregr.madlib.grps <- function(object, ...)
    lapply(residuals, object)
