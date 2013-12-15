## Implement marginal effects

margins <- function (model, vars = ~., at.mean = FALSE,
                     factor.continuous = FALSE, ...)
    UseMethod("margins", model)

## ----------------------------------------------------------------------

margins.lm.madlib <- function(model, vars = ~., at.mean = FALSE,
                              factor.continuous = FALSE, na.action = NULL,
                              ...)
{
    coef <- model$coef
    data <- model$data
    ## formula <- model$call$formula
    fake.data <- structure(vector("list", length(model$ind.vars)),
                           names = gsub("\"", "`", model$ind.vars),
                           class = "data.frame")
    f.terms <- terms(vars, fake.data)
    f.vars <- gsub("`", "\"", attr(f.terms, "term.labels"))
    if (any(! (f.vars %in% c(model$inde.vars, names(data))))
}

## ----------------------------------------------------------------------

.margins <- function(coef, data, P, var = NULL, at.mean = FALSE,
                     factor.continuous = FALSE)
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
                                     derv(P, i, coefs) %+% ")"))
                      })
        se <- array(0, dim = c(n,n))
        for (i in seq_len(n))
            se[i,] <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "with(avgs," %+%
                                            derv1(P, var[i], j, coefs)
                                            %+% ")"))
                             })
    } else {
        mar <- sapply(var,
                      function(i) {
                          eval(parse(text = "mean(with(data," %+%
                                     derv(P, i, coefs) %+% "))"))
                      })
        mar <- unlist(lk(.combine.list(mar), -1))
        for (i in seq_len(n)) {
            if (i == 1)
                se <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "mean(with(data," %+%
                                            derv1(P, var[i], j, coefs)
                                            %+% "))"))
                             })
            else
                se <- c(se, sapply(seq_len(n),
                                   function(j) {
                                       eval(parse(text = "mean(with(data," %+%
                                                  derv1(P, var[i], j, coefs)
                                                  %+% "))"))
                                   }))
        }
        se <- t(array(unlist(lk(.combine.list(se), -1)), dim = c(n,n)))
    }
    return(list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

## derivative over a variable
derv <- function(P, x, coefs)
{
    s <- .parse.deriv(P, x)
    w <- eval(parse(text = paste("substitute(", s, ", coefs)", sep = "")))
    as.character(enquote(w))[2]
}

## ----------------------------------------------------------------------

## derivative over a variable and a coefficient
derv1 <- function(P, x, j, coefs)
{
    s <- .parse.deriv(P, x)
    s1 <- .parse.deriv(s, "b" %+% "j")
    w <- eval(parse(text = paste("substitute(", s1, ", coefs)", sep = "")))
    as.character(enquote(w))[2]
}

