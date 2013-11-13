## ----------------------------------------------------------------------
## Coordinate descent method for elastic net
## Implemented in R space only
## ----------------------------------------------------------------------

## Gaussian coordinate descent method
.elnet.gaus.cd <- function (data, x, y, alpha, lambda, standardize, control,
                            glmnet, y.scl, y.ctr)
{
    n <- length(x)
    N <- nrow(data)
    ind.vars <- x
    x <- eval(parse(text = paste("with(data, c(",
                    paste(gsub("\"", "`", x), collapse = ", "), "))",
                    sep = "")))
    x <- Reduce(cbind, x[-1], x[[1]])
    y <- eval(parse(text = paste("with(data, ", gsub("\"", "`", y), ")",
                    sep = "")))
    if (glmnet) {
        tmp <- scale(x)
    } else {
        tmp <- scale(cbind(x, y))
    }
    centers <- attr(tmp, "scaled:center")
    sds <- attr(tmp, "scaled:scale")
    if (standardize) {
        if (glmnet) {
            x <- tmp * sqrt(N/(N-1))
            mx <- centers
            my <- 0
            sx <- sds * sqrt((N-1)/N)
        } else {
            x <- tmp[-(n+1)] * sqrt(N/(N-1))
            y <- tmp[n+1] * sds[n+1]
            mx <- centers[-(n+1)]
            my <- centers[n+1]
            sx <- sds[-(n+1)] * sqrt((N-1)/N)
        }
    } else {
        if (glmnet) {
            my <- 0
            mx <- centers
            sx <- 1
        } else {
            my <- tail(centers, 1)
            mx <- centers[-(n+1)]
            sx <- 1
        }
    }
    xx <- lk(crossprod(x))
    xy <- lk(crossprod(x, y))
    coef <- rep(0, n+1) # including the intercept
    rst <- .Call("elcd", as.matrix(xx), as.vector(xy), mx, my, sx, alpha,
                 lambda, standardize, control$use.active.set,
                 as.integer(control$max.iter), control$tolerance,
                 as.integer(N), coef, PACKAGE = "PivotalR")
    intercept <- coef[n+1]
    coef <- coef[1:n]
    if (glmnet) {
        intercept <- intercept * y.scl + y.ctr
        coef <- coef * y.scl
    }

    rst <- list(coef = coef, intercept = intercept)
    rows <- gsub("\"", "", ind.vars)
    ## rst$ind.vars <- rows
    col.name <- gsub("\"", "", data@.col.name)
    appear <- data@.appear.name
    for (i in seq_len(length(col.name))) 
        if (col.name[i] != appear[i])
            rows <- gsub(col.name[i], appear[i], rows)
    rows <- gsub("\\((.*)\\)\\[(\\d+)\\]", "\\1[\\2]", rows)
    names(rst$coef) <- rows
    names(rst$intercept) <- "(Intercept)"
    rst
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
