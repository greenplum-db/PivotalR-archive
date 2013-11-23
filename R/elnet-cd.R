## ----------------------------------------------------------------------
## Coordinate descent method for elastic net
## Implemented in R space only
## ----------------------------------------------------------------------

## Gaussian coordinate descent method
.elnet.gaus.cd <- function (data, x, y, alpha, lambda, standardize, control,
                            glmnet, params, call)
{
    if (is.null(params$verbose) || params$verbose)
        message("Warning: The coordinate descent algorithm implemented ",
                "here may not work very well when the number of features ",
                "is larger than a couple of thousands!")
    n <- length(x)
    N <- nrow(data)
    ind.vars <- x
    x <- eval(parse(text = paste("with(data, c(",
                    paste(gsub("\"", "`", x), collapse = ", "), "))",
                    sep = "")))
    x <- Reduce(cbind, x[-1], x[[1]])
    y <- eval(parse(text = paste("with(data, ", gsub("\"", "`", y), ")",
                    sep = "")))
    tmp <- scale(cbind(x, y))
    centers <- attr(tmp, "scaled:center")
    sds <- attr(tmp, "scaled:scale")
    if (standardize) {
        if (glmnet) {
            mx <- centers[-(n+1)]
            y.ctr <- centers[n+1]
            my <- 0
            x <- tmp[-(n+1)] * sqrt(N/(N-1))
            y <- tmp[n+1] * sqrt(N/(N-1))
            y.scl <- sds[n+1] * sqrt((N-1)/N)
            sx <- sds[-(n+1)] * sqrt((N-1)/N)
            sy <- 1
            lambda <- lambda / y.scl
        } else {
            mx <- centers[-(n+1)]
            my <- centers[n+1]
            x <- tmp[-(n+1)] * sqrt(N/(N-1))
            y <- y - my
            sx <- sds[-(n+1)] * sqrt((N-1)/N)
            sy <- sds[n+1] * sqrt((N-1)/N)
        }
    } else {
        if (glmnet) {
            my <- 0
            mx <- centers[-(n+1)]
            y <- tmp[n+1] * sqrt(N/(N-1))
            y.scl <- sds[n+1] * sqrt((N-1)/N)
            y.ctr <- centers[n+1]
            sx <- 1
            sy <- 1
            lambda <- lambda / y.scl
        } else {
            my <- tail(centers, 1)
            mx <- centers[-(n+1)]
            sx <- 1
            sy <- sds[n+1] * sqrt((N-1)/N)
        }
    }
    compute <- cbind(crossprod(x), crossprod(x, y))
    compute <- as.db.data.frame(compute, verbose = FALSE)
    xx <- compute[,1]; class(xx) <- "db.Rcrossprod"; xx@.dim <- c(n,n)
    xx@.is.symmetric <- TRUE; xx <- as.matrix(lk(xx))
    xy <- compute[,2]; class(xy) <- "db.Rcrossprod"; xy@.dim <- c(1,n)
    xy@.is.symmetric <- FALSE; xy <- as.vector(lk(xy))
    delete(compute)
    coef <- rep(0, n+1) # including the intercept
    iter <- 0
    loglik <- 0
    rst <- .Call("elcd", xx, xy, mx, my, sx, sy,
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
.elnet.binom.cd <- function (data, x, y, alpha, lambda, standardize, control,
                             params, call)
{
    if (is.null(params$verbose) || params$verbose)
        message("Warning: The coordinate descent algorithm implemented ",
                "here may not work very well when the number of features ",
                "is larger than a couple of thousands!")
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
    out.iter <- 0
    inner.iter <- 0
    repeat {
        newton <- .update.newton(x, y, coef, alpha, lambda, N, control)
        diff <- abs(coef - newton$coef)
        diff[coef != 0] <- diff[coef != 0] / coef[coef != 0]
        diff <- mean(diff)
        out.iter <- out.iter + 1
        inner.iter <- inner.iter + newton$iter
        coef <- newton$coef
        if (diff <= control$tolerance) break
    }

    ## prepare the result
    intercept <- coef[n+1]
    coef <- coef[1:n]
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
    rst$iter <- c(out.iter, inner.iter)
    rst$loglik <- .elnet.binom.loglik(coef, intercept, x, y, alpha, lambda)
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
    rst$family <- "binomial"
    class(rst) <- "elnet.madlib"
    rst
}

## ----------------------------------------------------------------------

.update.newton <- function (x, y, coef, alpha, lambda, N, control)
{
    n <- length(coef) - 1 # exclude the intercept
    intercept <- coef[n+1]
    coef <- coef[1:n]
    mid <- cbind(x, as.integer(y))
    names(mid) <- c(names(mid)[1:n], "y")
    mid$lin <- intercept + Reduce(function(l,r) l+r, as.list(coef*x))
    mid$p <- 1 / (1 + exp(-1 * mid$lin))
    f <- as.db.data.frame(mid, is.view = TRUE, verbose = FALSE)
    w <- with(f, p * (1 - p))
    z <- with(f, lin + (y - p) / (p * (1 - p)))
    x <- f[,1:n]
    y <- as.numeric(f[,n+1])
    compute <- cbind(crossprod(x, w*x), crossprod(w*x, y),
                     mean(cbind(w * x, x, y)))
    compute <- as.db.data.frame(compute, verbose = FALSE)
    xx <- compute[,1]; class(xx) <- "db.Rcrossprod"; xx@.dim <- c(n,n)
    xx@.is.symmetric <- FALSE; xx <- as.matrix(lk(xx))
    xy <- compute[,2]; class(xy) <- "db.Rcrossprod"; xy@.dim <- c(n,1)
    xy@.is.symmetric <- FALSE; xy <- as.vector(lk(xy))
    ms <- unlist(lk(compute[,3]))
    mwx <- ms[1:n]
    mx <- ms[1:n + n]
    my <- tail(ms, 1)
    iter <- 0
    rst <- .Call("elcd_binom", xx, xy, mwx, mx, my,
                 alpha, lambda, control$use.active.set,
                 as.integer(control$max.iter),
                 control$tolerance, as.integer(N), coef, iter,
                 PACKAGE = "PivotalR")
    delete(f)
    delete(compute)
    list(coef = coef, iter = iter)
}

## ----------------------------------------------------------------------

.elnet.binom.loglik <- function(coef, intercept, x, y, alpha, lambda)
{
    conn.id <- conn.id(x)
    coef.str <- "array[" %+% ("," %.% coef) %+% "]"
    x.str <- "array[" %+% ("," %.% x@.expr) %+% "]"
    y.str <- y@.expr
    if (x@.source == x@.parent)
        tbl <- x@.parent
    else
        tbl <- "(" %+% x@.parent %+% ") s"
    madlib <- schema.madlib(conn.id) # MADlib schema name
    sql <- paste("select avg(", madlib,
                 ".__elastic_net_binomial_loglikelihood(", coef.str, ", ",
                 intercept, ", ", x.str, ", ", y.str, ")) as loss from ",
                 tbl, sep = "")
    loss <- .get.res(sql, conn.id = conn.id)
    -(loss + lambda*((1-alpha)*sum(coef^2)/2 + alpha*sum(abs(coef))))
}
