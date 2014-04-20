## ----------------------------------------------------------------------
## Coordinate descent method for elastic net
## Implemented in R space only
## ----------------------------------------------------------------------

## Gaussian coordinate descent method
.elnet.gaus.cd <- function (data, x, y, alpha, lambda, standardize, control,
                            glmnet, params, call)
{
    if (is.null(control$control$verbose) || control$control$verbose)
        warning("The coordinate descent algorithm implemented ",
                "here may not work very well when the number of features ",
                "is larger than a couple of thousands!")
    warnings <- .suppress.warnings(conn.id(data))
    n <- length(x)
    N <- nrow(data)
    ind.vars <- x
    vdata <- .expand.array(data)
    vdata <- data
    x <- eval(parse(text = paste("with(vdata, c(",
                    paste(gsub("\"", "`", x), collapse = ", "), "))",
                    sep = "")))
    ## x <- Reduce(cbind2, x[-1], x[[1]])
    x <- .combine.list(x)
    y <- eval(parse(text = paste("with(vdata, ", gsub("\"", "`", y), ")",
                    sep = "")))
    tmp <- scale(cbind2(x, y))
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
            y.scl <- 1
        }
        ## mid <- as.db.data.frame(cbind2(db.array(x), y), verbose = FALSE)
        ## x <- mid[,1]
        ## y <- mid[,2]
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
            y.scl <- 1
        }
    }
    compute <- cbind2(crossprod(x), crossprod(x, y))
    compute <- as.db.data.frame(compute, verbose = FALSE)
    xx <- compute[,1]; class(xx) <- "db.Rcrossprod"; xx@.dim <- c(n,n)
    xx@.is.symmetric <- TRUE; xx <- as.matrix(lk(xx))
    xy <- compute[,2]; class(xy) <- "db.Rcrossprod"; xy@.dim <- c(1,n)
    xy@.is.symmetric <- FALSE; xy <- as.vector(lk(xy))
    delete(compute)
    coef <- rep(0, n+1) # including the intercept
    ## coef <- rnorm(n+1, 0, 1e-6)
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
    rows <- gsub("\"", "", params$ind.vars)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)
    rst$ind.vars <- rows
    col.name <- gsub("\"", "", data@.col.name)
    appear <- data@.appear.name
    for (i in seq_len(length(col.name)))
        if (col.name[i] != appear[i])
            rows <- gsub(col.name[i], appear[i], rows)
    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    rows <- gsub("\\s", "", rows)
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
    rst$col.name <- gsub("\"", "", data@.col.name)
    rst$method <- "cd"
    rst$family <- "gaussian"
    rst$max.iter <- control$max.iter
    rst$tolerance <- rst$tolerance
    rst$factor.ref <- data@.factor.ref
    class(rst) <- "elnet.madlib"
    ## if (standardize) delete(mid)
    .restore.warnings(warnings)
    rst
}

## ----------------------------------------------------------------------

## Binomial coordinate descent method
.elnet.binom.cd <- function (data, x, y, alpha, lambda, standardize, control,
                             params, call)
{
    if (is.null(control$control$verbose) || control$control$verbose)
        warning("The coordinate descent algorithm implemented ",
                "here may not work very well when the number of features ",
                "is larger than a couple of thousands!")
    warnings <- .suppress.warnings(conn.id(data))
    n <- length(x)
    N <- nrow(data)
    ind.vars <- x
    vdata <- .expand.array(data)
    vdata <- data
    x <- eval(parse(text = paste("with(vdata, c(",
                    paste(gsub("\"", "`", x), collapse = ", "), "))",
                    sep = "")))
    ## x <- Reduce(cbind2, x[-1], x[[1]])
    x <- .combine.list(x)
    y <- eval(parse(text = paste("with(vdata, ", gsub("\"", "`", y), ")",
                    sep = "")))
    tmp <- scale(x)
    centers <- attr(tmp, "scaled:center")
    sds <- attr(tmp, "scaled:scale")
    if (standardize) {
        x <- tmp * sqrt(N/(N-1))
        mx <- centers
        sx <- sds * sqrt((N-1)/N)
        mid <- as.db.data.frame(cbind2(db.array(x), y), verbose = FALSE)
        x <- mid[,1]
        y <- mid[,2]
    } else {
        mx <- centers
        sx <- 1
    }

    ## tx <- x
    ## tx$const <- 1
    ## ty <- as.integer(y)
    ## ty[y==1,] <- 10
    ## ty[y==0,] <- -10
    ## xx <- lk(crossprod(tx))
    ## xy <- lk(crossprod(tx,ty))
    ## sol <- as.numeric(solve(xx) %*% xy)
    ## print(sol)

    ## coef <- sol
    coef <- rep(0, n+1)
    ## coef <- rnorm(n+1, 0, 1e-6)
    iter <- 0
    loglik <- 0
    out.iter <- 0
    inner.iter <- 0
    if (!is.null(control$control$warmup) && control$control$warmup) {
        if (!is.null(control$control$warmup.lambda.no))
            lno <- control$control$warmup.lambda.no
        else
            lno <- 5
        if (lno != as.integer(lno))
            stop("warmup.lambda.no must be an integer!")
        lambdas <- c(exp(seq(log(1e2 * lambda), log(lambda + 1e-4),
                             length.out = lno)), lambda)
    } else {
        lambdas <- lambda
    }
    lc <- 1
    repeat {
        prev <- coef; prev[1] <- 0; prev[1] <- coef[1]
        ## prev <- rep(0, length(coef))
        ## for (i in seq_len(length(prev))) prev[i] <- coef[i]
        newton <- .update.newton(x, y, coef, alpha, lambdas[lc], N, control)
        out.iter <- out.iter + 1
        inner.iter <- inner.iter + newton$iter
        diff <- abs(prev - newton$coef)
        diff[prev != 0] <- diff[prev != 0] / coef[prev != 0]
        diff <- mean(diff)
        coef <- newton$coef
        if (diff <= control$tolerance || inner.iter > control$max.iter) {
            if (lc == length(lambdas) || inner.iter > control$max.iter) break
            else lc <- lc + 1
        }
    }

    ## prepare the result
    intercept <- coef[n+1]
    coef <- coef[1:n]
    rst <- list(coef = coef, intercept = intercept)
    rows <- gsub("\"", "", params$ind.vars)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)
    rst$ind.vars <- rows
    col.name <- gsub("\"", "", data@.col.name)
    appear <- data@.appear.name
    for (i in seq_len(length(col.name)))
        if (col.name[i] != appear[i])
            rows <- gsub(col.name[i], appear[i], rows)
    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    rows <- gsub("\\s", "", rows)
    names(rst$coef) <- rows
    names(rst$intercept) <- "(Intercept)"
    rst$iter <- c(out.iter, inner.iter)

    rst$loglik <- .elnet.binom.loglik(coef, intercept, x, y, alpha, lambda)

    if (standardize) {
        rst$coef <- rst$coef / sx
        rst$intercept <- rst$intercept - sum(rst$coef * mx)
    }

    rst$glmnet <- FALSE
    rst$y.scl <- 1
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
    rst$col.name <- gsub("\"", "", data@.col.name)
    rst$method <- "cd"
    rst$family <- "binomial"
    rst$max.iter <- control$max.iter
    rst$tolerance <- control$tolerance
    rst$factor.ref <- data@.factor.ref
    class(rst) <- "elnet.madlib"
    if (standardize) delete(mid)
    .restore.warnings(warnings)
    rst
}

## ----------------------------------------------------------------------

.convert.to.double.array <- function (x)
{
    as.numeric(strsplit(gsub("^\\{(.*?)\\}$", "\\1", x), ",")[[1]])
}

## ----------------------------------------------------------------------

.update.newton <- function (x, y, coef, alpha, lambda, N, control)
{
    n <- length(coef) - 1 # exclude the intercept
    intercept <- coef[n+1]
    rcoef <- coef[1:n]

    mid <- as.integer(y)
    names(mid) <- "y"

    mid$lin <- intercept + rowSums(rcoef * x)

    mid$p <- 1 / (1 + exp(-1 * mid$lin))
    mid$x <- db.array(x)

    ## mid$w[mid$p < 1e-5 | mid$p > 1 - 1e-5] <- 1e-5
    ## mid$p[mid$p < 1e-5] <- 0
    ## mid$p[mid$p > 1 - 1e-5] <- 1

    ## f <- as.db.data.frame(mid, is.view = FALSE, verbose = FALSE)
    ## f <- as.db.data.frame(mid, is.view = TRUE, verbose = FALSE)
    f <- as.db.Rview(mid)

    ## w <- f$w
    x <- f$x
    w <- f$p * (1 - f$p)
    wx <- w * f$x
    z <- f$lin + (f$y - f$p) / w
    wz <- w * f$lin + f$y - f$p

    compute <- Reduce(cbind2, c(crossprod(x, wx), crossprod(wx, z),
                                mean(Reduce(cbind2, c(wx, x, z, wz, w)))))

    ## compute <- as.db.data.frame(compute, verbose = FALSE)
    ## xx <- compute[,1]; class(xx) <- "db.Rcrossprod"; xx@.dim <- c(n,n)
    ## xx@.is.symmetric <- FALSE; xx <- as.matrix(lk(xx))
    ## xy <- compute[,2]; class(xy) <- "db.Rcrossprod"; xy@.dim <- c(1,n)
    ## xy@.is.symmetric <- FALSE; xy <- as.vector(lk(xy))

    compute <- lk(compute, array = FALSE)
    xx <- .convert.to.double.array(compute[,1])
    xx <- array(xx, dim = c(n,n))
    xy <- .convert.to.double.array(compute[,2])

    ## ms <- unlist(lk(compute[,-c(1,2)]))
    ## mwx <- ms[1:n]
    ## mx <- ms[1:n + n]
    ## my <- ms[2*n+1]
    ## mwz <- ms[2*n+2]
    ## mw <- ms[2*n+3]
    mwx <- .convert.to.double.array(compute[,3])
    mx <- .convert.to.double.array(compute[,4])
    my <- compute[,5]
    mwz <- compute[,6]
    mw <- compute[,7]

    iter <- 0
    ## coef <- rep(0, n+1)

    rst <- .Call("elcd_binom", xx, xy, mwx, mx, mwz, mw,
                 alpha, lambda, control$use.active.set,
                 as.integer(control$max.iter),
                 control$tolerance, as.integer(N), coef, iter,
                 PACKAGE = "PivotalR")

    ## delete(f)
    ## delete(compute)
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
    where <- y@.where
    if (where != "") where.str <- paste(" where", where)
    else where.str <- ""
    sort <- y@.sort
    madlib <- schema.madlib(conn.id) # MADlib schema name
    sql <- paste("select avg(", madlib,
                 ".__elastic_net_binomial_loglikelihood(", coef.str, ", ",
                 intercept, ", (", y.str, ")::boolean, ", x.str,
                 ")) as loss from ",
                 tbl, where.str, sort$str, sep = "")
    loss <- as.numeric(db.q(sql, nrows = -1,
                            conn.id = conn.id, verbose = FALSE))
    -(loss + lambda*((1-alpha)*sum(coef^2)/2 + alpha*sum(abs(coef))))
}
