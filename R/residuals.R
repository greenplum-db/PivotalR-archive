## ----------------------------------------------------------------------
## Extract residual info
## ----------------------------------------------------------------------

.compute.resid <- function(object)
{
    (eval(parse(text=paste("with(object$data,",
                deparse(object$terms[[2]]), ")", sep = "")))
     - predict(object, object$data))
}

## ----------------------------------------------------------------------

residuals.glm.madlib <-
    residuals.lm.madlib <-
        residuals.logregr.madlib <- function(object, ...)
    .compute.resid(object)

## ----------------------------------------------------------------------

residuals.glm.madlib.grps <-
    residuals.lm.madlib.grps <-
        residuals.logregr.madlib.grps <- function(object, ...)
    lapply(object, residuals)
