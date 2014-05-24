## Implement marginal effects

margins <- function (model, dydx = ~ Vars(model), newdata = model$data,
                     at.mean = FALSE, factor.continuous = FALSE,
                     na.action = NULL, ...)
    UseMethod("margins", model)

## ----------------------------------------------------------------------

.prepare.ind.vars <- function(ind.vars)
{
    vars <- gsub("::[\\w\\s]+", "", ind.vars, perl = T)
    vars <- gsub("\"", "`", vars, perl = T)
    vars <- gsub("\\(`([^\\[\\]]*?)`\\)\\[(\\d+?)\\]", "`\"\\1\"[\\2]`",
                 vars, perl = T)
    vars <- gsub("\\s", "", vars, perl = T)
    vars <- .reverse.consistent.func(vars)
    vars
}

## ----------------------------------------------------------------------

## list the independent variables indices
Terms <- function(term = NULL)
{
    if (is.null(term)) term
    else as.integer(term)
}

## ----------------------------------------------------------------------

Vars <- function(model)
{
    if (is.null(model$ind.vars))
        model.vars <- .prepare.ind.vars(model[[1]]$ind.vars)
    else
        model.vars <- .prepare.ind.vars(model$ind.vars)
    unique(all.vars(parse(text = model.vars)))
}

## ----------------------------------------------------------------------

## Add `\"\"` quotes to vars using model.vars
.add.quotes <- function(vars, model.vars)
{
    no.conflict.names <- sapply(seq_along(model.vars), function(i) .unique.string())
    parse.var <- function (v) {
        for (i in order(nchar(model.vars), decreasing = TRUE))
            v <- gsub(model.vars[i], paste("`\"", no.conflict.names[i], "\"`", sep = ""),
                      v, perl = TRUE)
        for (i in seq_along(model.vars))
            v <- gsub(no.conflict.names[i], model.vars[i], v, perl = TRUE)
        v <- gsub("([^`]|^)\"([^\\[\\]]*?)\"\\[(\\d+?)\\]([^`]|$)",
                  "(`\"\\2\"[\\3]`)", v, perl = TRUE)
        v
    }
    as.vector(sapply(vars, parse.var))
}

## ----------------------------------------------------------------------

.parse.margins.vars <- function(model, newdata, vars)
{
    coef <- model$coef
    data <- model$data
    model.vars <- .prepare.ind.vars(model$ind.vars)
    n <- length(model.vars)

    fake.data <- structure(vector("list", length(names(newdata))),
                                  names = names(newdata),
                                  class = "data.frame")
    f.terms <- terms(formula(paste(
        rownames(attr(model$terms, "factors"))[1],
        paste(deparse(vars), collapse=""))), data = fake.data)
    f.vars <- gsub("\\s", "", attr(f.terms, "term.labels"))

    is.terms <- grepl("^Terms\\(.*\\)$", f.vars)
    vars.terms <- f.vars[is.terms]
    select.ind <- integer(0)
    for (i in vars.terms) {
        idx <- eval(parse(text = i))
        if (is.null(idx)) idx <- seq_len(n)
        select.ind <- c(select.ind, idx)
    }
    select.ind <- unique(select.ind)

    f.vars <- f.vars[!is.terms]

    is.vars <- grepl("^Vars\\(.*\\)$", f.vars)
    vars.terms <- f.vars[is.vars]
    select.vars <- character(0)
    for (i in vars.terms) {
        idx <- eval(parse(text = i))
        select.vars <- c(select.vars, idx)
    }
    select.vars <- unique(select.vars)
    f.vars <- unique(c(select.vars, f.vars[!is.vars]))

    is.ind <- rep(FALSE, length(f.vars))
    if (length(select.ind) != 0) {
        f.vars <- c(f.vars, paste("term.", select.ind, sep = ""))
        is.ind <- c(is.ind, rep(TRUE, length(select.ind)))
    }

    expand.vars <- character(0)
    expand.is.ind <- logical(0)
    for (i in seq_len(length(f.vars))) {
        if (f.vars[i] %in% names(newdata) &&
            newdata[[f.vars[i]]]@.col.data_type == "array") {
            l <- array.len(newdata[[f.vars[i]]])
            expand.vars <- c(expand.vars, paste("`", f.vars[i], "[",
                                                seq_len(l), "]`", sep = ""))
            expand.is.ind <- c(expand.is.ind, rep(is.ind[i], l))
        } else if (grepl("^[^\\[\\]]*\\[[^\\[\\]]*\\]$",
                         f.vars[i], perl = T)) {
            var <- gsub("`", "", f.vars[i])
            x.str <- gsub("([^\\[\\]]*?)\\[[^\\[\\]]*?\\]", "\\1", var,
                           perl = TRUE)
            idx <- gsub("[^\\[\\]]*?\\[([^\\[\\]]*?)\\]", "\\1", var,
                         perl = TRUE)
            idx <- eval(parse(text = idx))
            expand.vars <- c(expand.vars, paste("`\"", .strip(x.str, "\""),
                                                "\"[", idx, "]`", sep = ""))
            expand.is.ind <- c(expand.is.ind, rep(is.ind[i], length(idx)))
        } else {
            expand.vars <- c(expand.vars, f.vars[i])
            expand.is.ind <- c(expand.is.ind, is.ind[i])
        }
    }

    ## ------- Deal with factors -------

    ## analyze the factor names
    factors <- character(0)
    used.vars <- model$dummy
    used.is.factor <- rep(FALSE, length(used.vars))
    for (i in seq_len(length(used.vars))) {
        parse.var <- .extract.factor.info(used.vars[i])
        if (length(parse.var) == 2) {
            factors <- rbind(
                factors,
                c(parse.var,
                  paste("`\"", used.vars[i], "\"`", sep = ""),
                  model$factor.ref[model$col.name == parse.var[1]]))
            used.is.factor[i] <- TRUE
        }
    }

    l <- length(expand.vars)
    is.factor <- rep(FALSE, l)
    if (sum(used.is.factor) != 0) {
        factors <- cbind(factors, sapply(
            seq_len(nrow(factors)),
            function(i) {
                sub <- factors[factors[,1] == factors[i,1],]
                paste("\"", factors[i,1],
                      gsub(paste(".*(", .unique.pattern.short(), ").*", sep = ""),
                           "\\1", factors[i,3], perl = TRUE),
                      factors[i,4], "\"", sep = "")
            }))

        remove <- rep(FALSE, l)
        append <- character(0)
        existing.factors <- apply(factors, 1,
                                  function(x) paste(x[1:2], collapse = "."))
        for (i in seq_len(l)) {
            j <- which(used.vars == expand.vars[i])
            if (identical(j, integer(0))) {
                if (expand.vars[i] %in% c(factors[,1], existing.factors)) {
                    if (expand.vars[i] %in% factors[,1]) {
                        k <- which(factors[,1] == expand.vars[i]
                                   & factors[,2] != factors[,4])
                        remove[i] <- TRUE
                        append <- c(append, gsub("\"", "",
                                                 gsub("`", "", factors[k,3])))
                    } else {
                        is.factor[i] <- TRUE
                        idx <- (existing.factors == expand.vars[i] &
                                factors[,2] != factors[,4])
                        if (all(idx == FALSE))
                            stop("The marginal effect for teh reference ",
                                 "category is not to be computed!")
                        expand.vars[i] <- gsub(
                            "\"", "", gsub("`", "",factors[idx,3]))
                    }
                }
            } else {
                if (used.is.factor[j]) is.factor[i] <- TRUE
                sub <- factors[gsub("\"", "",
                                    gsub("`", "", factors[,3]))
                               == used.vars[j],]
                if (gsub("`", "", sub[3]) == sub[5]) remove[i] <- TRUE
            }
        }

        expand.vars <- expand.vars[!remove]
        is.factor <- is.factor[!remove]
        expand.is.ind <- expand.is.ind[!remove]
        if (!identical(append, character(0))) {
            expand.vars <- c(expand.vars, append)
            is.factor <- c(is.factor, rep(TRUE, length(append)))
            expand.is.ind <- c(expand.is.ind, rep(FALSE, length(append)))
        }
    }

    ## ------- End -------

    ## ------- Add quotes -------
    mvars <- Vars(model)
    model.vars <- .add.quotes(gsub("`", "", model.vars), mvars)
    model.vars <- lapply(model.vars, function(x)
                         eval(parse(text=paste("quote(", x, ")", sep = ""))))

    names(model.vars) <- paste("\"term.", seq_len(n), "\"", sep = "")

    if (any(! (gsub("\"", "", gsub("`", "", expand.vars)) %in%
               c(gsub("\"", "", gsub("`", "", names(model.vars))),
               names(.expand.array(data)), model$dummy))))
        stop("All the variables must be in the independent variables ",
             "or the table column names!")

    expand.vars <- paste("\"", expand.vars, "\"", sep = "")
    expand.vars <- gsub("\"`\"([^\\[\\]]*?)\"\\[([^\\[\\]]*?)\\]`\"", "\"\\1\"[\\2]",
                        expand.vars, perl = TRUE)
    expand.vars <- gsub("\"`([^\\[\\]]*?)\\[([^\\[\\]]*?)\\]`\"", "\"\\1\"[\\2]",
                        expand.vars, perl = TRUE)

    return (list(vars = expand.vars, is.ind = expand.is.ind,
                 is.factor = is.factor, factors = factors,
                 model.vars = model.vars))
}

## ----------------------------------------------------------------------

margins.lm.madlib <- function(model, dydx = ~ Vars(model),
                              newdata = model$data,
                              at.mean = FALSE, factor.continuous = FALSE,
                              ## at = list(),
                              na.action = NULL, ...)
{
    ## stopifnot(inherits(at, "list"))

    if (!is.null(model$na.action)) na.action <- model$na.action
    if (model$num_missing_rows_skipped > 0) na.action <- na.omit
    if (!is.null(na.action)) {
        vars <- unique(gsub(paste(.unique.pattern.short(),
                                  ".*$", sep = ""), "",
                            Vars(model), perl = T))
        newdata <- na.action(
            newdata, vars = union(
                all.vars(parse(text = rownames(attr(model$terms, "factors"))[1])),
                vars))
    }

    vars <- dydx
    if (!is(newdata, "db.obj"))
        stop("newdata must be a db.obj object!")
    newdata <- .handle.dummy(newdata, model)
    f <- .parse.margins.vars(model, newdata, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- "b1 + " %+% paste("b", 2:n, "*(`\"term.", (2:n)-1, "\"`)",
                               collapse="+", sep = "")
    else
        P <- paste("b", 1:n, "*(`\"term.", 1:n, "\"`)", collapse="+", sep = "")
    if (at.mean) {
        avgs <- lk(mean(newdata))
        avgs <- .expand.avgs(avgs)
        names(avgs) <- paste("\"", gsub("_avg$", "", names(avgs)), "\"", sep = "")
        names(avgs) <- gsub("([^`]|^)\"([^\\[\\]]*?)_avg\\[(\\d+?)\\]\"([^`]|$)",
                            "\"\\2\"[\\3]", names(avgs))
    ## } else if (length(at) > 0) {
    ##     at.mean <- TRUE
    ##     avgs <- at
    ##     for (i in seq_len(length(at))) {
    ##         if (! names(at)[i] %in% names(newdata) &&
    ##             ! names(at)[i] %in% f$factors[,3])
    ##     }
    ##     names(avgs) <- paste("\"", .strip(names(avgs), "\""), "\"", sep = "")
    } else
        avgs <- NULL

    res <- .margins.lin(P, model, model$coef, newdata, f$vars, f$is.ind,
                        f$is.factor, f$model.vars, f$factors,
                        na.action, at.mean, factor.continuous, avgs = avgs)

    ## re-arrange the order of results
    ## in res, non-factor results are all in front of factor results
    if (factor.continuous) {
        mar <- res$mar
        se <- sqrt(res$se)
    } else {
        mar <- rep(0, length(res$mar))
        mar[!f$is.factor] <- res$mar[seq_len(sum(!f$is.factor))]
        mar[f$is.factor] <- res$mar[sum(!f$is.factor) +
                                    seq_len(sum(f$is.factor))]
        se0 <- sqrt(res$se)
        se <- rep(0, length(res$mar))
        se[!f$is.factor] <- se0[seq_len(sum(!f$is.factor))]
        se[f$is.factor] <- se0[sum(!f$is.factor) + seq_len(sum(f$is.factor))]
    }

    t <- mar / se
    p <- 2 * (1 - pt(abs(t), nrow(newdata) - n))
    rows <- gsub("`", "", f$vars)
    rows <- .strip(rows, "\"")
    rows <- ifelse(f$is.ind, "."%+%rows, rows)
    for (i in seq_len(length(rows[f$is.factor]))) {
        rows[f$is.factor][i] <- paste(
            f$factors[.strip(.strip(f$factors[,3], "`"), "\"") ==
                      rows[f$is.factor][i], 1:2],
            collapse = ".")
    }
    rows <- gsub("([^\\[\\]]*?)\"\\[(\\d+?)\\]", "\\1[\\2]", rows)

    res <- data.frame(cbind(Estimate = mar, `Std. Error` = se, `t value` = t,
                            `Pr(>|t|)` = p), row.names = rows,
                      check.names = FALSE)
    class(res) <- c("margins", "data.frame")
    res
}

## ----------------------------------------------------------------------

margins.lm.madlib.grps <- function(model, dydx = ~ Vars(model),
                                   newdata = lapply(model, function(x) x$data),
                                   at.mean = FALSE, factor.continuous = FALSE,
                                   ## at = list(),
                                   na.action = NULL, ...)
{
    if (length(newdata) == 1)
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], dydx, newdata, at.mean,
                       factor.continuous, na.action, ...))
    else if (length(newdata) == length(model))
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], dydx, newdata[[i]], at.mean,
                       factor.continuous, na.action, ...))
    else
        stop("The number db.obj objects in newdata must be 1 or equal to length of model!")
}

## ----------------------------------------------------------------------

margins.logregr.madlib <- function(model, dydx = ~ Vars(model),
                                   newdata = model$data,
                                   at.mean = FALSE, factor.continuous = FALSE,
                                   ## at = list(),
                                   na.action = NULL, ...)
{
    ## stopifnot(inherits(at, "list"))

    if (!is.null(model$na.action)) na.action <- model$na.action
    if (model$num_missing_rows_skipped > 0) na.action <- na.omit
    if (!is.null(na.action)) {
        vars <- unique(gsub(paste(.unique.pattern.short(),
                                  ".*$", sep = ""), "",
                            Vars(model), perl = T))
        newdata <- na.action(
            newdata, vars = union(
                all.vars(parse(text = rownames(attr(model$terms, "factors"))[1])),
                vars))
    }

    vars <- dydx
    if (!is(newdata, "db.obj"))
        stop("newdata must be a db.obj object!")
    newdata <- .handle.dummy(newdata, model)
    f <- .parse.margins.vars(model, newdata, vars)
    n <- length(model$coef)
    if (model$has.intercept)
        P <- "b1 +" %+% paste("b", 2:n, "*(`\"term.", (2:n)-1, "\"`)",
                                  collapse="+", sep = "")
    else
        P <- paste("b", 1:n, "*(`\"term.", 1:n, "\"`)", collapse="+", sep = "")
    sigma <- paste("1/(1+exp(-(", P, ")))")
    sigma.name <- gsub("__", "", .unique.string())

    coefs <- as.list(model$coef)
    n <- length(coefs)
    m <- length(vars)
    names(coefs) <- paste("b", seq_len(n), sep = "")
    avgs <- NULL
    cmd <- paste("substitute(", sigma, ", c(f$model.vars, coefs))", sep = "")
    expr <- gsub("\\s", "", paste(deparse(eval(parse(text = cmd))), collapse = ""))

    if (at.mean) {
        avgs <- lk(mean(newdata))
        avgs <- .expand.avgs(avgs)
        names(avgs) <- gsub("_avg$", "", names(avgs))
        names(avgs) <- paste("\"", gsub("_avg$", "", names(avgs)), "\"", sep = "")
        names(avgs) <- gsub("([^`]|^)\"([^\\[\\]]*?)_avg\\[(\\d+?)\\]\"([^`]|$)",
                            "\"\\2\"[\\3]", names(avgs))
        expr <- gsub("\\s", "",
                     paste(deparse(eval(parse(text = paste("substitute(",
                                              expr, ", avgs)", sep = "")))),
                           collapse = ""))
        avgs[[sigma.name]] <- eval(parse(text = expr))
    ## } else if (length(at) > 0) {
    ##     at.mean <- TRUE
    ##     avgs <- at
    ##     names(avgs) <- paste("\"", .strip(names(avgs), "\""), "\"", sep = "")
    } else {
        newdata[[sigma.name]] <- .with.data(newdata, expr)
        newdata <- as.db.Rview(newdata)
    }

    res <- .margins.log(P, model, model$coef, newdata, f$vars, f$is.ind,
                        f$is.factor, f$model.vars, sigma.name, f$factors,
                        na.action, at.mean, factor.continuous, avgs = avgs)

    ## re-arrange the order of results
    ## in res, non-factor results are all in front of factor results
    if (factor.continuous) {
        mar <- res$mar
        se <- sqrt(res$se)
    } else {
        mar <- rep(0, length(res$mar))
        mar[!f$is.factor] <- res$mar[seq_len(sum(!f$is.factor))]
        mar[f$is.factor] <- res$mar[sum(!f$is.factor) +
                                    seq_len(sum(f$is.factor))]
        se0 <- sqrt(res$se)
        se <- rep(0, length(res$mar))
        se[!f$is.factor] <- se0[seq_len(sum(!f$is.factor))]
        se[f$is.factor] <- se0[sum(!f$is.factor) + seq_len(sum(f$is.factor))]
    }

    z <- mar / se
    p <- 2 * (1 - pnorm(abs(z)))
    rows <- gsub("`", "", f$vars)
    rows <- .strip(rows, "\"")
    rows <- ifelse(f$is.ind, "."%+%rows, rows)
    for (i in seq_len(length(rows[f$is.factor]))) {
        rows[f$is.factor][i] <- paste(
            f$factors[.strip(.strip(f$factors[,3], "`"), "\"") ==
                                              rows[f$is.factor][i], 1:2],
                                    collapse = ".")
    }
    rows <- gsub("([^\\[\\]]*?)\"\\[(\\d+?)\\]", "\\1[\\2]", rows)

    res <- data.frame(cbind(Estimate = mar, `Std. Error` = se, `z value` = z,
                            `Pr(>|z|)` = p),
                      row.names = rows, check.names = FALSE)
    class(res) <- c("margins", "data.frame")
    res
}

## ----------------------------------------------------------------------

print.margins <- function(x,
                          digits = max(3L, getOption("digits") - 3L),
                          ...)
{
    printCoefmat(x, digits = digits, signif.stars = TRUE)
}

## ----------------------------------------------------------------------

margins.logregr.madlib.grps <- function(model, dydx = ~ Vars(model),
                                        newdata = lapply(model, function(x) x$data),
                                        at.mean = FALSE, factor.continuous = FALSE,
                                        ## at = list(),
                                        na.action = NULL, ...)
{
    if (length(newdata) == 1)
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], dydx, newdata, at.mean,
                       factor.continuous, na.action, ...))
    else if (length(newdata) == length(model))
        lapply(seq_len(length(model)), function (i)
               margins(model[[i]], dydx, newdata[[i]], at.mean,
                       factor.continuous, na.action, ...))
    else
        stop("The number db.obj objects in newdata must be 1 or equal to length of model!")
}
## ----------------------------------------------------------------------

.with.data <- function(data, expr.str, type = "double precision",
                       is.agg = FALSE)
{
    nm <- names(data)[1]
    x <- data[[nm]]
    x@.expr <- .consistent.func(expr.str)
    x@.col.name <- .unique.string()
    x@.col.data_type <- type
    x@.is.agg <- is.agg
    x@.content <- gsub("^select (.*) from",
                       paste("select ", x@.expr, " as ", x@.col.name, " from",
                             sep = ""),
                       x@.content)
    x
}

## ----------------------------------------------------------------------

## expand x into x[1], x[2], ...
.expand.avgs <- function(avgs)
{
    n <- ncol(avgs)
    res <- avgs
    for (i in seq_len(n)) {
        if (is.array(avgs[,i])) {
            a <- as.list(avgs[,i])
            names(a) <- paste(names(avgs)[i], "[", seq_len(length(avgs[,i])),
                              "]", sep = "")
            res <- c(res, a)
        }
    }
    res
}

## ----------------------------------------------------------------------

## create a db.Rview object which contains the dummy variable
.handle.dummy <- function(data, model)
{
    l <- length(model$dummy)
    if (l != 0) {
        data <- .db.data.frame2db.Rquery(data)
        data@.col.name <- c(data@.col.name, model$dummy)
        data@.expr <- c(data@.expr, model$dummy.expr)
        data@.col.data_type <- c(data@.col.data_type,
                                 rep("double precision", l))
        data@.col.udt_name <- c(data@.col.udt_name, rep("float8", l))
        if (data@.source == data@.parent)
            tbl <- data@.parent
        else
            tbl <- "(" %+% data@.parent %+% ") s"
        where <- data@.where
        if (where != "") where.str <- paste(" where", where)
        else where.str <- ""
        sort <- data@.sort
        data@.content <- paste("select ", paste(data@.expr, " as \"",
                                                data@.col.name, "\"",
                                                collapse = ", ", sep = ""),
                               " from ", tbl, where.str, sort$str, sep = "")
        as.db.Rview(data)
    } else {
        if (is(data, "db.data.frame"))
            data
        else
            as.db.Rview(data)
    }
}

## ----------------------------------------------------------------------

## special margins for linear
.margins.lin <- function(P, model, coef, data, vars, is.ind, is.factor,
                         model.vars, factors, na.action, at.mean = FALSE,
                         factor.continuous = FALSE, avgs = NULL)
{
    coefs <- as.list(coef)
    n <- length(coefs)
    m <- length(vars)
    names(coefs) <- paste("b", seq_len(n), sep = "")

    if (sum(is.factor) > 0 && !factor.continuous) {
        select.c <- seq_len(m)[!is.factor] # indices for continuous
        select.i <- seq_len(m)[is.factor] # indicies for indicators
    } else {
        select.c <- seq_len(m)
        select.i <- integer(0)
    }

    if (at.mean) {
        mar <- unlist(sapply(
            select.c,
            function(i) {
                eval(parse(text = "with(avgs," %+%
                           .sub.coefs(.dx(P, vars[i], is.ind[i], model.vars),
                                      coefs) %+% ")"))
            }))
        se <- array(0, dim = c(length(select.c),n))
        cnt <- 1
        for (i in select.c) {
            s <- .dx(P, vars[i], is.ind[i], model.vars)
            se[cnt,] <- sapply(
                seq_len(n),
                function(j) {
                    eval(parse(
                        text = paste("with(avgs,",
                        .sub.coefs(.parse.deriv(s, "b"%+%j), coefs),
                        ")", sep = "")))
                })
            cnt <- cnt + 1
        }

        mar.i <- unlist(sapply(
            select.i,
            function(i) {
                eval(parse(
                    text = paste("with(avgs, ",
                    .sub.coefs(.diff.lin(P, vars[i], model.vars,
                                         factors), coefs),
                    ")", sep = "")))
            }))
        se.i <- array(0, dim = c(length(select.i),n))
        cnt <- 1
        for (i in select.i) {
            s <- .diff.lin(P, vars[i], model.vars, factors)
            se.i[cnt,] <- sapply(
                seq_len(n),
                function(j) {
                    eval(parse(
                        text = paste("with(avgs, ",
                        .sub.coefs(.parse.deriv(s, "b"%+%j), coefs),
                        ")", sep = "")))
                })
            cnt <- cnt + 1
        }

        mar <- c(mar, mar.i)
        se <- rbind(se, se.i)
    } else {
        ## continuous variables
        mar <- NULL
        se <- NULL
        mar <- unlist(sapply(
            select.c,
            function(i) {
                .with.data(
                    data,
                    paste("avg(",
                          gsub("`", "",
                               .sub.coefs(.dx(P, vars[i], is.ind[i],
                                              model.vars), coefs)),
                          ")", sep = ""))

            }))
        for (i in select.c) {
            s <- .dx(P, vars[i], is.ind[i], model.vars)
            e <- sapply(
                seq_len(n),
                function(j) {
                    .with.data(data, paste(
                        "avg(", gsub("`", "", .sub.coefs(
                            .parse.deriv(s, "b"%+%j), coefs)),
                        ")", sep = ""), is.agg = TRUE)
                })
            if (i == 1)
                se <- e
            else
                se <- c(se, e)
        }

        ## factors treated as uncontinuous variables
        mar.i <- NULL
        se.i <- NULL
        mar.i <- unlist(sapply(
            select.i,
            function(i) {
                .with.data(
                    data,
                    paste("avg(",
                          gsub("`", "", .sub.coefs(
                              .diff.lin(P, vars[i], model.vars,
                                        factors), coefs)),
                          ")", sep = ""))
            }))

        for (i in select.i) {
            s <- .diff.lin(P, vars[i], model.vars, factors)
            e <- sapply(
                seq_len(n),
                function(j) {
                    .with.data(data, paste(
                        "avg(", gsub("`", "", .sub.coefs(
                            .parse.deriv(s, "b"%+%j), coefs)),
                        ")", sep = ""), is.agg = TRUE)
                })
            if (i == 1) {
                se.i <- e
            } else {
                se.i <- c(se.i, e)
            }
        }

        mar.se <- c(mar, mar.i, se, se.i)

        if (is.list(mar.se)) {
            mar.se <- .combine.list(mar.se)
            mar.se <- unlist(lk(mar.se, -1))
        }

        mar <- mar.se[seq_len(m)]
        se <- t(array(mar.se[-seq_len(m)], dim = c(n,m)))
    }
    names(mar) <- gsub("`", "", vars)
    v <- vcov(model, na.action)
    se <- diag(se %*% v %*% t(se))
    return (list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

.margins.log <- function(P, model, coef, data, vars, is.ind, is.factor,
                         model.vars, sigma, factors, na.action, at.mean = FALSE,
                         factor.continuous = FALSE, avgs = NULL)
{
    conn.id <- conn.id(data)
    coefs <- as.list(coef)
    n <- length(coefs)
    m <- length(vars)
    names(coefs) <- paste("b", seq_len(n), sep = "")

    if (sum(is.factor) > 0 && !factor.continuous) {
        select.c <- seq_len(m)[!is.factor] # indices for continuous
        select.i <- seq_len(m)[is.factor] # indicies for indicators
    } else {
        select.c <- seq_len(m)
        select.i <- integer(0)
    }

    if (at.mean) {
        mar <- unlist(sapply(
            select.c,
            function(i) {
                eval(parse(text = "with(avgs," %+%
                           .sub.coefs(.dx(P, vars[i], is.ind[i],
                                          model.vars), coefs) %+% ")"))
            }))
        mar <- mar * avgs[[sigma]] * (1 - avgs[[sigma]])

        se <- array(0, dim = c(length(select.c),n))
        cnt <- 1
        for (i in select.c) {
            s <- .dx(P, vars[i], is.ind[i], model.vars)
            se[cnt,] <- sapply(
                seq_len(n),
                function(j) {
                    (eval(parse(
                        text = paste("with(avgs,",
                        .sub.coefs(.parse.deriv(s, "b"%+%j), coefs),
                        ")", sep = "")))
                     * avgs[[sigma]] * (1 - avgs[[sigma]]) +
                     eval(parse(
                         text = paste("with(avgs,",
                         .sub.coefs(
                             paste("(", s, ")*(",
                                   .dj(P, j, model.vars), ")",
                                   sep = ""), coefs), ")", sep = "")))
                     * avgs[[sigma]] * (1 - avgs[[sigma]]) * (1 - 2*avgs[[sigma]]))
                })
            cnt <- cnt + 1
        }

        ## factor
        mar.i <- unlist(sapply(
            select.i,
            function(i) {
                eval(parse(
                    text = paste("with(avgs, ",
                    .sub.coefs(.diff.log(P, vars[i], model.vars, sigma,
                                         factors), coefs),
                    ")", sep = "")))
            }))

        se.i <- array(0, dim = c(length(select.i), n))
        cnt <- 1
        for (i in select.i) {
            se.i[cnt,] <- sapply(
                seq_len(n),
                function(j) {
                    eval(parse(
                        text = paste("with(avgs, ",
                        .sub.coefs(.diff.log.dj(P, vars[i], model.vars,
                                                sigma, j, factors), coefs),
                        ")", sep = "")))
                })
            cnt <- cnt + 1
        }
        mar <- c(mar, mar.i)
        se <- rbind(se, se.i)
    } else {
        madlib <- schema.madlib(conn.id(data))

        ## continuous variables
        if (length(select.c) > 0) {
            mar <- paste("array[", paste(sapply(
                select.c,
                function(i) {
                    .sub.coefs(.dx(P, vars[i], is.ind[i], model.vars), coefs)
                }), collapse = ", "), "]::double precision[]", sep = "")
            mar <- paste(madlib, ".avg(", madlib, ".array_scalar_mult(", mar,
                         ",", sigma, "*(1 - ", sigma, ")::double precision))",
                         sep = "")

            se1 <- paste(sapply(
                select.c,
                function(i) {
                    s <- .dx(P, vars[i], is.ind[i], model.vars)
                    paste(sapply(
                        seq_len(n),
                        function(j) {
                            .sub.coefs(.parse.deriv(s, "b"%+%j), coefs)
                        }), collapse = ", ")
                }), collapse = ", ")

            se2 <- paste(sapply(
                select.c,
                function(i) {
                    s <- .dx(P, vars[i], is.ind[i], model.vars)
                    paste(sapply(
                        seq_len(n),
                        function(j) {
                            .sub.coefs(paste("(", s, ")*(", .dj(P, j,
                                                                model.vars),
                                             ")", sep = ""), coefs)
                        }), collapse = ", ")
                }), collapse = ", ")

            se1 <- paste(madlib, ".avg(", madlib, ".array_scalar_mult(array[",
                         se1, "]::double precision[],", sigma,
                         "*(1 - ", sigma, ")::double precision))", sep = "")
            se2 <- paste(madlib, ".avg(", madlib, ".array_scalar_mult(array[",
                         se2, "]::double precision[],", sigma,
                         "*(1-", sigma, ")*(1-2*", sigma,
                         ")::double precision))", sep = "")
        }

        ## factors treated as incontinuous variables
        if (length(select.i) > 0) {
            mar.i <- paste(madlib, ".avg(array[", paste(sapply(
                select.i,
                function(i) {
                    .sub.coefs(.diff.log(P, vars[i], model.vars, sigma,
                                         factors), coefs)
                }), collapse = ", "), "]::double precision[])", sep = "")

            se.i <- paste(madlib, ".avg(array[", paste(sapply(
                select.i,
                function(i) {
                    ## s <- .diff.log(P, vars[i], model.vars, sigma)
                    paste(sapply(
                        seq_len(n),
                        function(j) {
                            ## .sub.coefs(.parse.deriv(s, "b"%+%j), coefs)
                            .sub.coefs(.diff.log.dj(P, vars[i], model.vars,
                                                    sigma, j, factors), coefs)
                        }), collapse = ", ")
                }), collapse = ", "), "]::double precision[])", sep = "")
        }

        ## combine all strings
        if (length(select.c) > 0 && length(select.i) > 0)
            mar.se <- paste(mar, " as mar, ", mar.i, " as mar_i, ",
                            se1, " as se1, ", se2, " as se2, ",
                            se.i, " as se_i ", sep = "")
        else if (length(select.c) > 0)
            mar.se <- paste(mar, " as mar, ", se1, " as se1, ",
                            se2, " as se2 ", sep = "")
        else
            mar.se <- paste(mar.i, " as mar_i, ", se.i, " as se_i ", sep = "")
        mar.se <- paste("select ", mar.se, "from (", data@.parent, ") s",
                        sep = "")
        mar.se <- gsub("`", "", mar.se)

        mar.se <- db.q(mar.se, conn.id=conn.id, verbose = FALSE)
        if (length(select.c) > 0 && length(select.i) > 0) {
            mar <- c(as.vector(arraydb.to.arrayr(mar.se$mar)),
                     as.vector(arraydb.to.arrayr(mar.se$mar_i)))
            se <- t(array(c(as.vector(arraydb.to.arrayr(mar.se$se1) +
                                arraydb.to.arrayr(mar.se$se2)),
                            as.vector(arraydb.to.arrayr(mar.se$se_i))),
                          dim = c(n,m)))
        } else if (length(select.c) > 0) {
            mar <- as.vector(arraydb.to.arrayr(mar.se$mar))
            se <- t(array(as.vector(arraydb.to.arrayr(mar.se$se1) +
                                      arraydb.to.arrayr(mar.se$se2)),
                            dim = c(n,m)))
        } else {
            mar <- as.vector(arraydb.to.arrayr(mar.se$mar_i))
            se <- t(array(as.vector(arraydb.to.arrayr(mar.se$se_i)),
                          dim = c(n,m)))
        }
    }
    names(mar) <- gsub("`", "", vars)
    v <- vcov(model, na.action)
    se <- diag(se %*% v %*% t(se))
    return(list(mar=mar, se=se))
}

## ----------------------------------------------------------------------

.dx <- function(P, x, is.ind, model.vars)
{
    if (is.ind) {
        s <- .parse.deriv(P, x)
        s <- paste(deparse(eval(parse(text = paste("substitute(", s,
                                      ", model.vars)")))), collapse = "")
        s <- gsub("\\n", "", s)
    } else {
        P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                      ", model.vars)", sep = "")))),
                   collapse = "")
        P <- gsub("\\n", "", P)
        x <- paste(deparse(eval(parse(text = paste("quote(", x, ")")))),
                   collapse = "")
        x <- gsub("\\n", "", x)
        s <- .parse.deriv(P, x)
    }
    s
}

## ----------------------------------------------------------------------

.get.friend.dummy <- function(x, factors)
{
    col <- factors[.strip(factors[,3], "`") == x, 1]
    .strip(factors[factors[,1] == col, 3], "`")
}

## ----------------------------------------------------------------------

## finite difference, only for factors, linear regression
.diff.lin <- function(P, x, model.vars, factors)
{
    P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                  ", model.vars)", sep = "")))),
               collapse = "")
    P <- gsub("\\n", "", P)
    xs <- .get.friend.dummy(x, factors)
    env <- as.list(rep(0, length(xs)))
    names(env) <- xs
    env[[x]] <- 1
    P1 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P1 <- gsub("\\n", "", P1)
    env[[x]] <- 0
    ref <- factors[gsub("`", "", factors[,3]) == x, 5]
    if (ref %in% names(env)) env[[ref]] <- 1
    P0 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P0 <- gsub("\\n", "", P0)
    paste("(", P1, ") - (", P0, ")", sep = "")
}

## ----------------------------------------------------------------------

## finite difference, only for factors, logistic regression
.diff.log <- function(P, x, model.vars, sigma, factors)
{
    P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                  ", model.vars)", sep = "")))),
               collapse = "")
    P <- gsub("\\n", "", P)
    xs <- .get.friend.dummy(x, factors)
    env <- as.list(rep(0, length(xs)))
    names(env) <- xs
    env[[x]] <- 1
    P1 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P1 <- gsub("\\n", "", P1)
    env[[x]] <- 0
    ref <- factors[gsub("`", "", factors[,3]) == x, 5]
    if (ref %in% names(env)) env[[ref]] <- 1
    P0 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P0 <- gsub("\\n", "", P0)
    paste("1/(1 + exp(-(", P1, "))) - 1/(1 + exp(-(", P0, ")))", sep = "")
}

## ----------------------------------------------------------------------

.diff.log.dj <- function(P, x, model.vars, sigma, j, factors)
{
    dj <- .dj(P, j, model.vars)
    P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                  ", model.vars)", sep = "")))),
               collapse = "")
    P <- gsub("\\n", "", P)
    xs <- .get.friend.dummy(x, factors)
    env <- as.list(rep(0, length(xs)))
    names(env) <- xs
    env[[x]] <- 1
    P1 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P1 <- gsub("\\n", "", P1)
    sigma1 <- paste("1/(1 + exp(-(", P1, ")))", sep = "")
    dj1 <- .parse.deriv(P1, "b"%+%j)
    dj1 <- paste("(", dj1, ")*(", sigma1,")*(1 - ", sigma1, ")", sep = "")

    env[[x]] <- 0
    ref <- factors[gsub("`", "", factors[,3]) == x, 5]
    if (ref %in% names(env)) env[[ref]] <- 1
    P0 <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                   ", env)", sep = "")))),
                collapse = "")
    P0 <- gsub("\\n", "", P0)
    sigma0 <- paste("1/(1 + exp(-(", P0, ")))", sep = "")
    dj0 <- .parse.deriv(P0, "b"%+%j)
    dj0 <- paste("(", dj0, ")*(", sigma0,")*(1 - ", sigma0, ")", sep = "")
    paste(dj1, " - ", dj0, sep = "")
}

## ----------------------------------------------------------------------

.dj <- function(P, j, model.vars)
{
    P <- paste(deparse(eval(parse(text = paste("substitute(", P,
                                  ", model.vars)", sep = "")))),
               collapse = "")
    P <- gsub("\\n", "", P)
    .parse.deriv(P, "b"%+%j)
}

## ----------------------------------------------------------------------

.sub.coefs <- function(s, coefs)
{
    w <- eval(parse(text = paste("substitute(", s, ", coefs)", sep = "")))
    res <- as.character(enquote(w))[2]
    ## res <- gsub("`", "", res)
    res <- gsub("\\n", "", res)
    res
}

## ----------------------------------------------------------------------

## extract factor column name and its value
.extract.factor.info <- function(var)
{
    unique.string <- .unique.pattern.short()
    if (grepl(unique.string, var, perl = TRUE)) {
        res1 <- gsub(paste("^(.*)", unique.string, ".*$", sep = ""), "\\1",
                     var, perl = TRUE)
        res2 <- gsub(paste("^.*", unique.string, "(.*)$", sep = ""), "\\1",
                     var, perl = TRUE)
        c(res1, res2)
    } else
        var
}
