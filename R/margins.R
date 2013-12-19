## Implement marginal effects

margins <- function (model, vars = ~., at.mean = FALSE,
                     factor.continuous = FALSE, na.action = NULL,
                     ...)
    UseMethod("margins", model)

## ----------------------------------------------------------------------

.prepare.ind.vars <- function(ind.vars)
{
    vars <- gsub("::[\\w\\s]+", "", ind.vars, perl = T)
    vars <- gsub("\"", "`", vars)
    vars <- gsub("\\(`(.*)`\\)\\[(\\d+)\\]", "`\\1[\\2]`", vars)
    vars <- gsub("\\s", "", vars)
    .reverse.consistent.func(vars)
}

## ----------------------------------------------------------------------

.parse.margins.vars <- function(model, vars)
{
    coef <- model$coef
    data <- model$data
    ## formula <- model$call$formula
    ## model.vars <- model$origin.ind
    model.vars <- .prepare.ind.vars(model$ind.vars)
    fake.data <- structure(vector("list", length(model.vars)),
                           names = gsub("`", "", model.vars),
                           class = "data.frame")
    f.terms <- terms(vars, data = fake.data)
    ## f.vars <- gsub("`", "\"", attr(f.terms, "term.labels"))
    f.vars <- gsub("\\s", "", attr(f.terms, "term.labels"))
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

margins.lm.madlib <- function(model, vars = ~., newdata = model$data,
                              at.mean = FALSE, factor.continuous = FALSE,
                              na.action = NULL, ...)
{
    f <- .parse.margins.vars(model, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- "b1 + " %+% paste("b", 2:n, "*var", (2:n)-1, collapse="+",
                               sep = "")
    else
        P <- paste("b", 1:n, "*var", 1:n, collapse="+", sep = "")
    res <- .margins(model, model$coef, newdata, P, f$vars, "1", .deriv.eunit,
                    .unique.string(), f$model.vars, at.mean, factor.continuous)
    res
}

## ----------------------------------------------------------------------

margins.lm.madlib.grps <- function(model, vars = ~.,
                                   newdata = lapply(model, function(x) x$data),
                                   at.mean = FALSE, factor.continuous = FALSE,
                                   na.action = NULL, ...)
{
    if (length(newdata) == 1) 
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], vars, newdata, at.mean,
                       factor.continuous, na.action, ...))
    else if (length(newdata) == length(model))
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], vars, newdata[[i]], at.mean,
                       factor.continuous, na.action, ...))
    else
        stop("The number db.obj objects in newdata must be 1 or equal to length of model!")
}

## ----------------------------------------------------------------------

margins.logregr.madlib <- function(model, vars = ~., newdata = model$data,
                                   at.mean = FALSE, factor.continuous = FALSE,
                                   na.action = NULL, ...)
{
    f <- .parse.margins.vars(model, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        unit <- ("exp(-1*(b1 + " %+% paste("b", 2:n, "*var", (2:n)-1,
                                           collapse="+", sep = "")
                 %+% "))")
    else
        unit <- ("exp(-1*(" %+% paste("b", 1:n, "*var", 1:n,
                                      collapse="+", sep = "")
                 %+% "))")
    unit.name <- .unique.string()
    P <- "1/(1 + " %+% unit.name %+% ")"
    
    coefs <- as.list(model$coef)
    n <- length(coefs)
    m <- length(vars)
    names(coefs) <- paste("b", seq_len(n), sep = "")
    
    newdata[[unit.name]] <- with(newdata, eval(parse(text = paste("substitute(", unit,
                                                     ", c(f$model.vars, coefs))"))))
    newdata <- as.db.Rview(newdata)
    res <- .margins(model, model$coef, newdata, P, f$vars, unit, unit.name,
                    .deriv.eunit, f$model.vars, at.mean, factor.continuous)
    res
}

## ----------------------------------------------------------------------

## derivative of exp function
.deriv.eunit <- function(unit, unit.name)
{
    if (unit == "1") return ("0")
    inner <- gsub("^exp\\((.*)\\)$", "\\1", unit)
    paste("(", inner, ")*", unit.name, sep = "")
}

## ----------------------------------------------------------------------

margins.logregr.madlib.grps <- function(model, vars = ~.,
                                        newdata = lapply(model, function(x) x$data),
                                        at.mean = FALSE, factor.continuous = FALSE,
                                        na.action = NULL, ...)
{
    if (length(newdata) == 1) 
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], vars, newdata, at.mean,
                       factor.continuous, na.action, ...))
    else if (length(newdata) == length(model))
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], vars, newdata[[i]], at.mean,
                       factor.continuous, na.action, ...))
    else
        stop("The number db.obj objects in newdata must be 1 or equal to length of model!")
}

## ----------------------------------------------------------------------

.margins <- function(model, coef, data, P, vars, unit, unit.name,
                     deriv.unit, model.vars, at.mean = FALSE,
                     factor.continuous = FALSE)
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
                                     derv(P, i, unit, unit.name, deriv.unit,
                                          model.vars, coefs) %+% ")"))
                      })
        se <- array(0, dim = c(m,n))
        for (i in seq_len(m)) {
            s <- .derv.var(P, vars[i], unit, unit.name, deriv.unit, model.vars)
            se[i,] <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "with(avgs," %+%
                                            derv1(s, j, unit, unit.name, deriv.unit,
                                                  coefs)
                                            %+% ")"))
                             })
        }
    } else {
        mar <- sapply(vars,
                      function(i) {
                          eval(parse(text = "mean(with(data," %+%
                                     derv(P, i, unit, unit.name, deriv.unit,
                                          model.vars, coefs) %+% "))"))
                      })
        ## if (any(sapply(mar, function(s) is(s, "db.obj"))))
        ##     mar <- unlist(lk(.combine.list(mar), -1))
        for (i in seq_len(m)) {
            s <- .derv.var(P, vars[i], unit, unit.name, deriv.unit, model.vars)
            if (i == 1)
                se <- sapply(seq_len(n),
                             function(j) {
                                 eval(parse(text = "mean(with(data," %+%
                                            derv1(s, j, unit, unit.name, deriv.unit,
                                                  coefs)
                                            %+% "))"))
                             })
            else
                se <- c(se, sapply(seq_len(n),
                                   function(j) {
                                       eval(parse(text = "mean(with(data," %+%
                                                  derv1(s, j, unit, unit.name, deriv.unit,
                                                        coefs)
                                                  %+% "))"))
                                   }))
        }
        mar.se <- c(mar, se)
        if (any(sapply(mar.se, function(s) is(s, "db.obj"))))
            mar.se <- unlist(lk(.combine.list(mar.se), -1))
        mar <- mar.se[seq_len(m)]
        se <- t(array(mar.se[-seq_len(m)], dim = c(n,m)))
    }
    names(mar) <- gsub("`", "", vars)
    v <- vcov(model)
    se <- se %*% v %*% t(se)
    return(list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

.derv.var <- function(P, x, unit, unit.name, deriv.unit, model.vars)
{
    mv <- gsub("\\s", "",
               gsub("`", "", sapply(model.vars, function(s)
                                    paste(.strip(deparse(s), "\\n"),
                                          collapse=""))))
    xv <- gsub("`", "", x)
    if (xv %in% mv) {
        i <- which(mv == xv)
        s <- paste(.parse.deriv(P, "var" %+% i), "+(",
                   .parse.deriv(P, unit.name), ")*(",
                   .parse.deriv(deriv.unit(unit, unit.name),
                                "var" %+% i), ")", sep = "")
        s <- paste(deparse(eval(parse(text = paste("substitute(", s,
                                      ", model.vars)")))), collapse = "")
        s <- gsub("\\n", "", s)
    } else {
        P <- paste(deparse(eval(parse(text = paste("substitute(",
                                      paste(P, "+(", .parse.deriv(P, unit.name), ")*(",
                                            deriv.unit(unit, unit.name), sep = ""),   
                                      ", model.vars)")))), collapse = "")
        P <- gsub("\\n", "", P)
        x <- paste(deparse(eval(parse(text = paste("quote(", x, ")")))),
                   collapse = "")
        x <- gsub("\\n", "", x)
        s <- .parse.deriv(P, x)
    }
    s
}

## ----------------------------------------------------------------------

## derivative over a variable
derv <- function(P, x, unit, unit.name, deriv.unit, model.vars, coefs)
{
    s <- .derv.var(P, x, unit, unit.name, deriv.unit, model.vars)
    w <- eval(parse(text = paste("substitute(", s, ", coefs)", sep = "")))
    w <- gsub("`", "", as.character(enquote(w))[2])
    w <- gsub("-\\s*\\(", "-1*(", w)
    w
}

## ----------------------------------------------------------------------

## derivative over a variable and a coefficient
derv1 <- function(s, j, unit, unit.name, deriv.unit, coefs)
{
    ## s <- derv(P, x, model.vars, coefs)
    s1 <- paste(.parse.deriv(s, "b" %+% j), "+(", .parse.deriv(s, unit.name),
                ")*(", .parse.deriv(deriv.unit(unit, unit.name), "b" %+% j), ")",
                sep = "")
    w <- eval(parse(text = paste("substitute(", s1, ", coefs)", sep = "")))
    w <- gsub("`", "", as.character(enquote(w))[2])
    w <- gsub("-\\s*\\(", "-1*(", w)
    w
}

