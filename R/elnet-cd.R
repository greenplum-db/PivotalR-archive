## ----------------------------------------------------------------------
## Coordinate descent method for elastic net
## Implemented in R space only
## ----------------------------------------------------------------------

## Gaussian coordinate descent method
.elnet.gaus.cd <- function (data, x, y, alpha, lambda, standardize, control,
                            glmnet, y.scl, y.ctr, params, call)
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
    iter <- 0
    loglik <- 0
    rst <- .Call("elcd", as.matrix(xx), as.vector(xy), mx, my, sx, y.scl,
                 alpha, lambda, standardize, control$use.active.set,
                 as.integer(control$max.iter), control$tolerance,
                 as.integer(N), coef, iter, loglik, PACKAGE = "PivotalR")
    intercept <- coef[n+1]
    coef <- coef[1:n]
    if (glmnet) {
        intercept <- intercept * y.scl + y.ctr
        coef <- coef * y.scl
    }

    rst <- list(coef = coef, intercept = intercept)
    rows <- gsub("\"", "", ind.vars)
    rst$ind.vars <- rows
    col.name <- gsub("\"", "", data@.col.name)
    appear <- data@.appear.name
    for (i in seq_len(length(col.name))) 
        if (col.name[i] != appear[i])
            rows <- gsub(col.name[i], appear[i], rows)
    rows <- gsub("\\((.*)\\)\\[(\\d+)\\]", "\\1[\\2]", rows)
    names(rst$coef) <- rows
    names(rst$intercept) <- "(Intercept)"
    rst$iter <- iter
    rst$loglik <- loglik
    rst$glmnet <- glmnet
    rst$y.scl <- y.scl
    rst$standardize <- standardize
    rst$ind.str <- params$ind.str
    rst$dummy <- data@.dummy
    rst$dummy.expr <- data@.dummy.expr
    rst$appear <- appear
    rst$terms <- params$terms
    rst$model <- NA
    rst$call <- call
    rst$alpha <- alpha
    rst$lambda <- lambda
    rst$method <- "cd"
    rst$family <- "gaussian"
    class(rst) <- "elnet.madlib"
    rst
}

## ----------------------------------------------------------------------

## Binomial coordinate descent method
.elnet.binom.cd <- function (data, x, y, alpha, lambda, standardize, control)
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
    tmp <- scale(x)
    centers <- attr(tmp, "scaled:center")
    sds <- attr(tmp, "scaled:scale")
    if (standardize) {
        x <- tmp * sqrt(N/(N-1))
        mx <- centers
        sx <- sds * sqrt((N-1)/N)
    } else {
        mx <- centers
        sx <- 1
    }
    ## xx <- lk(crossprod(x))
    ## xy <- lk(crossprod(x, y))
    coef <- rep(0, n+1)
    iter <- 0
    loglik <- 0
    repeat {
        newton <- .update.newton(x, y, coef)
        diff <- abs(coef - newton)
        diff[coef != 0] <- diff[coef != 0] / coef[coef != 0]
        diff <- mean(diff)
        if (diff <= control$tolerance) break
    }

    ## prepare the result
}

## ----------------------------------------------------------------------

.update.newton <- function (x, y, coef, alpha, lambda)
{
    n <- length(coef) - 1 # exclude the intercept
    intercept <- coef[n+1]
    coef <- coef[1:n]
    mid <- cbind(x, y)
    mid$lin <- intercept + Reduce(function(l,r) l+r, as.list(coef*x))
    mid$p <- 1 / (1 + exp(-1 * mid$lin))
    f <- as.db.data.frame(mid, is.view = TRUE)
    w <- with(f, p * (1 - p))
    z <- with(f, lin + (y - p) / (p * (1 - p)))
    compute <- lk(cbind(crossprod(x, w*x), crossprod(w*x, y),
                        mean(cbind(w * x, x, y))))
    xx <- compute[[1]]
    xy <- compute[[2]]
    ms <- compute[[3]]
    mwx <- ms[1:n]
    mx <- ms[1:n + n]
    my <- last(ms, 1)
    rst <- .Call("elcd_binom", as.matrix(xx), as.vector(xy), mwx, mx, my
                 alpha, lambda, control$use.active.set, control$max.iter,
                 control$tolerance, coef, iter, PACKAGE = "PivotalR")
    delete(f)
    coef
}
