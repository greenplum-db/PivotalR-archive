## Implement marginal effects

margins <- function (model, vars = ~., at.mean = FALSE,
                     factor.continuous = FALSE, ...)
    UseMethod("margins", model)

## ----------------------------------------------------------------------

.parse.margins.vars <- function(model)
{
    coef <- model$coef
    data <- model$data
    ## formula <- model$call$formula
    model.vars <- gsub("\"", "`", model$ind.vars)
    fake.data <- structure(vector("list", length(model$ind.vars)),
                           names = model.vars,
                           class = "data.frame")
    f.terms <- terms(vars, fake.data)
    ## f.vars <- gsub("`", "\"", attr(f.terms, "term.labels"))
    if (any(! (f.vars %in% c(model.vars, names(data)))))
        stop("All the variables must be in the independent variables ",
             " or the table column names!")
    model.vars <- lapply(model.vars, function(x)
                         eval(parse(text="quote(", x, ")")))
    names(model.vars) <- paste("var", seq_len(length(model.vars)), sep = "")
    return (list(vars = f.vars, model.vars = model.vars))
}

## ----------------------------------------------------------------------

margins.lm.madlib <- function(model, vars = ~., at.mean = FALSE,
                              factor.continuous = FALSE, na.action = NULL,
                              ...)
{
    f <- .parse.margins.vars(model)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- "b1 + " %+% paste("b", 2:n, "*var", (2:n)-1, collapse="+",
                               sep = "")
    else
        P <- paste("b", 1:n, "*var", 1:n, collapse="+", sep = "")
    res. <- .margins(model$coef, model$data, P, f$vars, f$model.vars,
                     at.mean, factor.continuous)
    res
}

## ----------------------------------------------------------------------

margins.lm.madlib.grp <- function(model, vars = ~., at.mean = FALSE,
                                  factor.continuous = FALSE, na.action = NULL,
                                  ...)
    lapply(model, margins, vars, at.mean, factor.continuous, na.action, ...)

## ----------------------------------------------------------------------

.margins <- function(coef, data, P, vars, model.vars,
                     at.mean = FALSE, factor.continuous = FALSE)
{
    coefs <- coef
    n <- length(coefs)
    names(coefs) <- paste("b", seq_len(n), sep = "")
    if (at.mean) {
        avgs <- lk(mean(data))
        names(avgs) <- gsub("_avg$", "", names(avgs))
        mar <- sapply(var,
                      function(i) {
                          eval(parse(text = "with(avgs," %+%
                                     derv(P, i, model.vars, coefs) %+% ")"))
                      })
        se <- array(0, dim = c(n,n))
        for (i in seq_len(n))
            se[i,] <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "with(avgs," %+%
                                            derv1(P, var[i], j, model.vars,
                                                  coefs)
                                            %+% ")"))
                             })
    } else {
        mar <- sapply(var,
                      function(i) {
                          eval(parse(text = "mean(with(data," %+%
                                     derv(P, i, model.vars, coefs) %+% "))"))
                      })
        mar <- unlist(lk(.combine.list(mar), -1))
        for (i in seq_len(n)) {
            if (i == 1)
                se <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "mean(with(data," %+%
                                            derv1(P, var[i], j, model.vars,
                                                  coefs)
                                            %+% "))"))
                             })
            else
                se <- c(se, sapply(seq_len(n),
                                   function(j) {
                                       eval(parse(text = "mean(with(data," %+%
                                                  derv1(P, var[i], j,
                                                        model.vars, coefs)
                                                  %+% "))"))
                                   }))
        }
        se <- t(array(unlist(lk(.combine.list(se), -1)), dim = c(n,n)))
    }
    return(list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

## derivative over a variable
derv <- function(P, x, model.vars, coefs)
{
    if (x %in% model.vars) {
        i <- which(model.vars == x)
        s <- .parse.margins.vars(P, "var" %+% i)
        s <- deparse(eval(parse(text = paste("substitute(", s,
                                ", model.vars)"))))
    } else {
        P <- deparse(eval(parse(text = paste("substitute(", P,
                                ", model.vars)"))))
        x <- deparse(eval(parse(text = paste("quote(", x, ")"))))
        s <- .parse.deriv(P, x)
    }
    ## s <- .parse.deriv(P, x)
    w <- eval(parse(text = paste("substitute(", s, ", coefs)", sep = "")))
    as.character(enquote(w))[2]
}

## ----------------------------------------------------------------------

## derivative over a variable and a coefficient
derv1 <- function(P, x, j, model.vars, coefs)
{
    s <- derv(P, x, model.vars, coefs)
    s1 <- .parse.deriv(s, "b" %+% "j")
    w <- eval(parse(text = paste("substitute(", s1, ", coefs)", sep = "")))
    as.character(enquote(w))[2]
}

