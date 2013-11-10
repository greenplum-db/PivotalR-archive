## ----------------------------------------------------------------------
## Coordinate descent method for elastic net
## Implemented in R space only
## ----------------------------------------------------------------------

## Gaussian coordinate descent method
.elnet.gaus.cd <- function (data, x, y, alpha, lambda, standardize, control)
{
    n <- length(x)
    x <- eval(parse(text = paste("with(data, c(",
                    paste(x, collapse = ", "), "))", sep = "")))
    x <- db.array(Reduce(cbind, x[-1], x[[1]]))
    y <- eval(parse(text = paste("with(data, ", y, ")", sep = "")))
    tmp <- scale(cbind(x, y))
    if (standardize) {
        x <- tmp[-length(names(tmp))]
        y <- tmp[length(names(tmp))]
        mx <- 0
        my <- 0
    } else {
        centers <- attr(tmp, "scaled:center")
        my <- tail(centers, 1)
        mx <- centers[-length(centers)]
    }
    xx <- lk(crossprod(x))
    xy <- lk(crossprod(x, y))
    coef <- rep(0, n+1) # including the intercept
    intercept <- 0
    rst <- .Call("elcd", xx, xy, mx, my, alpha, lambda, standardize,
                 control$use.active.set, control$max.iter, control$tolerance,
                 nrow(data), coef, PACKAGE = "PivotalR")
    
}

## ----------------------------------------------------------------------

## Binomial coordinate descent method
.elnet.binom.cd <- function (data, x, y, alpha, lambda, standardize, control)
{
    n <- length(x)
    x <- eval(parse(text = paste("with(data, c(",
                    paste(x, collapse = ", "), "))", sep = "")))
    x <- db.array(Reduce(cbind, x[-1], x[[1]]))
    y <- eval(parse(text = paste("with(data, ", y, ")", sep = "")))
    xx <- lk(crossprod(x))
    xy <- lk(crossprod(x, y))
    coef <- rep(0, n)
    intercept <- 0
    rst <- .Call("elcd", xx, xy, alpha, lambda, standardize,
          control$use.active.set, control$max.iter, control$tolerance,
          coef, intercept, PACKAGE = "PivotalR")
}
