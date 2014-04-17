## ----------------------------------------------------------------------
## vcov methods for regressions
## ----------------------------------------------------------------------

.pseudo.inv <- function(X, tol = sqrt(.Machine$double.eps))
{
    ## Generalized Inverse of a Matrix
    dnx <- dimnames(X)
    if(is.null(dnx)) dnx <- vector("list", 2)
    s <- svd(X)
    nz <- s$d > tol * s$d[1]
    structure(if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
              dimnames = dnx[2:1])
}

## ----------------------------------------------------------------------

.extract.regr.x <- function(object, na.action)
{
    x <- object$data
    for (i in seq_len(length(object$dummy))) {
        x[[object$dummy[i]]] <- 1
        z <- x[[object$dummy[i]]]
        z@.expr <- object$dummy.expr[i]
        z@.content <- gsub("^select 1 as",
                           paste("select ", z@.expr, " as", sep = ""),
                           z@.content)
        x[[object$dummy[i]]] <- z
    }
    ## res <- Reduce(cbind2, eval(parse(text = "with(x, c(" %+%
    ##                                 ("," %.% object$ind.vars) %+% "))")))
    model.vars <- gsub("::[\\w\\s]+", "", object$ind.vars, perl = T)
    model.vars <- gsub("\"", "`", model.vars)
    model.vars <- gsub("\\(`([^\\[\\]]*)`\\)\\[(\\d+)\\]", "\\1[\\2]", model.vars)
    model.vars <- .reverse.consistent.func(model.vars)
    res <- .combine.list(eval(parse(text = "with(x, c(" %+% (","
                                    %.% model.vars) %+% "))")))
    if (object$has.intercept) {
        z <- res
        intercept <- .unique.string()
        z[[intercept]] <- 1
        res <- cbind2(z[[intercept]], res)
    }

    y <- setdiff(all.vars(parse(text = rownames(attr(object$terms, "factors"))[1])), names(res))
    if (!is.null(object$na.action)) na.action <- object$na.action
    if (!is.null(na.action)) {
        x.names <- names(res)
        for (i in y) res[[i]] <- object$data[[i]]
        res <- na.action(res, names(res))
        res <- res[, x.names]
    } else if (object$num_missing_rows_skipped > 0) {
        res <- na.omit(res, names(res))
        for (i in y) res <- res[!is.na(object$data[[i]]), ]
    }
    res
}

## ----------------------------------------------------------------------

vcov.lm.madlib <- function(object, na.action = NULL, ...)
{
    x <- .extract.regr.x(object, na.action)
    n <- nrow(object$data)
    k <- length(names(x))
    compute <- cbind2(mean(residuals(object)^2), crossprod(x))
    compute <- as.db.data.frame(compute, verbose = FALSE)
    mn <- lk(compute[,1]) * n / (n - k)
    xx <- compute[,2]; class(xx) <- "db.Rcrossprod"; xx@.dim <- c(k,k)
    xx@.is.symmetric <- TRUE; xx <- .pseudo.inv(lk(xx))
    delete(compute)
    res <- mn * xx

    model.vars <- gsub("::[\\w\\s]+", "", object$ind.vars, perl = T)
    model.vars <- gsub("\"", "`", model.vars)
    model.vars <- gsub("\\(`(.*)`\\)\\[(\\d+)\\]", "\\1[\\2]", model.vars)
    model.vars <- .reverse.consistent.func(model.vars)
    if (object$has.intercept)
        rows <- c("(Intercept)", model.vars)
    else
        rows <- model.vars
    for (i in seq_len(length(object$col.name)))
        if (object$col.name[i] != object$appear[i])
            rows <- gsub(object$col.name[i], object$appear[i], rows)
    rows <- .strip(rows, "`")
    colnames(res) <- rows
    rownames(res) <- rows

    res
}

## ----------------------------------------------------------------------

vcov.lm.madlib.grps <- function(object, na.action = NULL, ...)
    lapply(object, vcov, na.action = na.action, ...)

## ----------------------------------------------------------------------

vcov.logregr.madlib <- function(object, na.action = NULL, ...)
{
    x <- .extract.regr.x(object, na.action)
    ## cx <- Reduce(function(l,r) l+r, as.list(object$coef * x))
    cx <- rowSums(object$coef * x)
    a <- 1/((1 + exp(-1*cx)) * (1 + exp(cx)))
    xx <- .pseudo.inv(lk(crossprod(x, a*x)))

    model.vars <- gsub("::[\\w\\s]+", "", object$ind.vars, perl = T)
    model.vars <- gsub("\"", "`", model.vars)
    model.vars <- gsub("\\(`(.*)`\\)\\[(\\d+)\\]", "\\1[\\2]", model.vars)
    model.vars <- .reverse.consistent.func(model.vars)
    if (object$has.intercept)
        rows <- c("(Intercept)", model.vars)
    else
        rows <- model.vars
    for (i in seq_len(length(object$col.name)))
        if (object$col.name[i] != object$appear[i])
            rows <- gsub(object$col.name[i], object$appear[i], rows)
    rows <- .strip(rows, "`")
    colnames(xx) <- rows
    rownames(xx) <- rows

    xx
}

## ----------------------------------------------------------------------

vcov.logregr.madlib.grps <- function(object, na.action = NULL, ...)
    lapply(object, vcov, na.action = na.action, ...)
