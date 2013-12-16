## Implement marginal effects

margins <- function (model, vars = ~., at.mean = FALSE,
                     factor.continuous = FALSE, na.action = NULL,
                     ...)
    UseMethod("margins", model)

## ----------------------------------------------------------------------

.parse.margins.vars <- function(model, vars)
{
    coef <- model$coef
    data <- model$data
    ## formula <- model$call$formula
    ## model.vars <- model$origin.ind
    model.vars <- gsub("::[\\w\\s]+", "", model$ind.vars, perl = T)
    model.vars <- gsub("\"", "`", model.vars)
    model.vars <- gsub("\\(`(.*)`\\)\\[(\\d+)\\]", "`\\1[\\2]`", model.vars)
    fake.data <- structure(vector("list", length(model.vars)),
                           names = gsub("`", "", model.vars),
                           class = "data.frame")
    f.terms <- terms(vars, data = fake.data)
    ## f.vars <- gsub("`", "\"", attr(f.terms, "term.labels"))
    f.vars <- attr(f.terms, "term.labels")
    if (any(! (gsub("`", "", f.vars) %in% c(gsub("`", "", model.vars),
                                            names(.expand.array(data))))))
        stop("All the variables must be in the independent variables ",
             " or the table column names!")
    model.vars <- lapply(model.vars, function(x)
                         eval(parse(text=paste("quote(", x, ")", sep = ""))))
    names(model.vars) <- paste("var", seq_len(length(model.vars)), sep = "")
    return (list(vars = f.vars, model.vars = model.vars))
}

## ----------------------------------------------------------------------

margins.lm.madlib <- function(model, vars = ~., at.mean = FALSE,
                              factor.continuous = FALSE, na.action = NULL,
                              ...)
{
    f <- .parse.margins.vars(model, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- "b1 + " %+% paste("b", 2:n, "*var", (2:n)-1, collapse="+",
                               sep = "")
    else
        P <- paste("b", 1:n, "*var", 1:n, collapse="+", sep = "")
    res <- .margins(model$coef, model$data, P, f$vars, f$model.vars,
                    at.mean, factor.continuous)
    res
}

## ----------------------------------------------------------------------

margins.lm.madlib.grps <- function(model, vars = ~., at.mean = FALSE,
                                   factor.continuous = FALSE,
                                   na.action = NULL, ...)
    lapply(model, margins, vars, at.mean, factor.continuous, na.action, ...)

## ----------------------------------------------------------------------

margins.logregr.madlib <- function(model, vars = ~., at.mean = FALSE,
                                   factor.continuous = FALSE,
                                   na.action = NULL, ...)
{
    f <- .parse.margins.vars(model, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- ("1/(1 + exp(-(b1 + " %+% paste("b", 2:n, "*var", (2:n)-1,
                                             collapse="+", sep = "")
              %+% ")))")
    else
        P <- ("1/(1 + exp(-(b1 + " %+% paste("b", 1:n, "*var", 1:n,
                                             collapse="+", sep = "")
              %+% ")))")
    res <- .margins(model$coef, model$data, P, f$vars, f$model.vars,
                    at.mean, factor.continuous)
    res
}

## ----------------------------------------------------------------------

margins.logregr.madlib.grps <- function(model, vars = ~., at.mean = FALSE,
                                        factor.continuous = FALSE,
                                        na.action = NULL, ...)
    lapply(model, margins, vars, at.mean, factor.continuous, na.action, ...)


## ----------------------------------------------------------------------

.margins <- function(coef, data, P, vars, model.vars,
                     at.mean = FALSE, factor.continuous = FALSE)
{
    coefs <- as.list(coef)
    n <- length(coefs)
    m <- length(vars)
    names(coefs) <- paste("b", seq_len(n), sep = "")
    if (at.mean) {
        avgs <- lk(mean(data))
        names(avgs) <- gsub("_avg$", "", names(avgs))
        mar <- sapply(vars,
                      function(i) {
                          eval(parse(text = "with(avgs," %+%
                                     derv(P, i, model.vars, coefs) %+% ")"))
                      })
        se <- array(0, dim = c(n,n))
        for (i in seq_len(n)) {
            s <- .derv.var(P, vars[i], model.vars)
            se[i,] <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "with(avgs," %+%
                                            derv1(s, j, coefs)
                                            %+% ")"))
                             })
        }
    } else {
        mar <- sapply(vars,
                      function(i) {
                          eval(parse(text = "mean(with(data," %+%
                                     derv(P, i, model.vars, coefs) %+% "))"))
                      })
        if (any(sapply(mar, function(s) is(s, "db.obj"))))
            mar <- unlist(lk(.combine.list(mar), -1))
        for (i in seq_len(m)) {
            s <- .derv.var(P, vars[i], model.vars)
            if (i == 1)
                se <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "mean(with(data," %+%
                                            derv1(s, j, coefs)
                                            %+% "))"))
                             })
            else
                se <- c(se, sapply(seq_len(n),
                                   function(j) {
                                       eval(parse(text = "mean(with(data," %+%
                                                  derv1(s, j, coefs)
                                                  %+% "))"))
                                   }))
        }
        if (any(sapply(se, function(s) is(s, "db.obj"))))
            se <- t(array(unlist(lk(.combine.list(se), -1)), dim = c(n,m)))
        else
            se <- t(array(se, dim = c(n,m)))
    }
    return(list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

.derv.var <- function(P, x, model.vars)
{
    mv <- gsub("`", "", sapply(model.vars, function(s)
                               paste(deparse(s), collapse="")))
    xv <- gsub("`", "", x)
    if (xv %in% mv) {
        i <- which(mv == xv)
        s <- .parse.deriv(P, "var" %+% i)
        s <- paste(deparse(eval(parse(text = paste("substitute(", s,
                                      ", model.vars)")))), collapse = "")
    } else {
        P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                      ", model.vars)")))), collapse = "")
        x <- paste(deparse(eval(parse(text = paste("quote(", x, ")")))),
                   collapse = "")
        s <- .parse.deriv(P, x)
    }
    s
}

## ----------------------------------------------------------------------

## derivative over a variable
derv <- function(P, x, model.vars, coefs)
{
    s <- .derv.var(P, x, model.vars)
    w <- eval(parse(text = paste("substitute(", s, ", coefs)", sep = "")))
    gsub("`", "", as.character(enquote(w))[2])
}

## ----------------------------------------------------------------------

## derivative over a variable and a coefficient
derv1 <- function(s, j, coefs)
{
    ## s <- derv(P, x, model.vars, coefs)
    s1 <- .parse.deriv(s, "b" %+% j)
    w <- eval(parse(text = paste("substitute(", s1, ", coefs)", sep = "")))
    gsub("`", "", as.character(enquote(w))[2])
}

