## -----------------------------------------------------------------------
## Wrapper function for MADlib's linear, logistic and multinomial
## logistic regressions
## -----------------------------------------------------------------------

setClass("logregr.madlib")
setClass("logregr.madlib.grps")
setClass("glm.madlib")
setClass("glm.madlib.grps")

## ------------------------------------------------------------

## define a new family function
## whose only use is to specify multinomial
## No other uses
multinomial <- function(link = 'logit')
{
    return (list(family = 'multinomial', link = 'logit'))
}

## ------------------------------------------------------------

## na.action is a place holder
## family specific parameters are in control, which
## is a list of parameters
madlib.glm <- function (formula, data,
                        family = gaussian,
                        na.action = NULL, control = list(), ...)
{
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }

    family.name <- gsub("\\.", "_", tolower(family$family))
    link.name <- if (family$link == "1/mu^2") "sqr_inverse" else family$link

    args <- control
    args$formula <- formula
    args$data <- data
    args$na.action <- na.action
    args$family.name <- family.name
    args$link.name <- link.name
    call <- match.call()

    ## must use GLM function
    use.glm <- !is.null(control$use.glm) && control$use.glm == TRUE

    ## linear regression
    if (family.name == "gaussian" && link.name == 'identity' && !use.glm)
    {
        fit <- do.call(madlib.lm, args)
        if (is(fit, "lm.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    ## logistic regression
    if (family.name == "binomial" && link.name == "logit" && !use.glm)
    {
        fit <- do.call(.madlib.logregr, args)
        if (is(fit, "logregr.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    ## multinomial logistic
    if (family.name == "multinomial" && link.name == 'logit' && !use.glm)
    {
        fit <- do.call(.madlib.mlogregr, args)
        if (is(fit, "mlogregr.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    ## All other cases are handled by MADlib's GLM function
    fit <- do.call(.madlib.glm, args)
    if (is(fit, 'glm.madlib')) fit$call <- call
    else
        for (i in seq_len(length(fit))) fit[[i]]$call <- call

    return (fit)
}

## -----------------------------------------------------------------------

.madlib.logregr <- function (formula, data, na.action = NULL, method = "irls",
                             max.iter = 10000, tolerance = 1e-5, verbose = FALSE,
                             na.as.level = FALSE, ...)
{
    ## make sure fitting to db.obj
    if (! is(data, "db.obj"))
        stop("madlib.glm can only be used on a db.obj object, and ",
             deparse(substitute(data)), " is not!")
    origin.data <- data

    ## Only newer versions of MADlib are supported
    .check.madlib.version(data)

    warnings <- .suppress.warnings(conn.id(data))
    analyzer <- .get.params(formula, data, na.action, na.as.level)
    data <- analyzer$data
    params <- analyzer$params
    is.tbl.source.temp <- analyzer$is.tbl.source.temp
    tbl.source <- analyzer$tbl.source
    db <- .get.dbms.str(conn.id(data))
    ## dependent, independent and grouping strings
    if (is.null(params$grp.str))
        grp <- "NULL::text"
    else
        if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
            stop("MADlib on HAWQ 1.1 does not support grouping ",
                 "in logistic regression !")
        else
            grp <- paste("'", params$grp.str, "'", sep = "")

    ## construct SQL string
    conn.id <- conn.id(data)
    ## tbl.source <- gsub("\"", "", content(data))
    tbl.source <- content(data)

    if (.madlib.version.number(conn.id) <= 0.7) {
        tbl.source <- gsub("\"", "", tbl.source)
    }

    madlib <- schema.madlib(conn.id) # MADlib schema name
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str)) {
        tbl.output <- NULL
        sql <- paste("select (f).* from (select ", madlib,
                     ".logregr('", tbl.source, "', '",
                     gsub("'", "''", params$dep.str),
                     "', '", params$ind.str, "', ", max.iter,
                     ", '", method, "', ", tolerance, ",",
                     verbose, ") as f) s",
                     sep = "")
    } else {
        tbl.output <- .unique.string()
        sql <- paste("select ", madlib, ".logregr_train('",
                     tbl.source, "', '", tbl.output, "', '",
                     gsub("'", "''", params$dep.str),
                     "', '", params$ind.str, "', ",
                     grp, ", ", max.iter, ", '", method, "', ",
                     tolerance, ", ", verbose, ")", sep = "")
    }

    ## execute the logistic regression and get the result
    res <- db.q(sql, "; select * from ", tbl.output, nrows = -1,
                conn.id = conn.id, verbose = FALSE)

    ## drop temporary tables
    ## if (!is.null(tbl.output)) .db.removeTable(tbl.output, conn.id)
    if (is.tbl.source.temp) delete(tbl.source, conn.id)

    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        model <- NULL
    else
        model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)

    .restore.warnings(warnings)

    ## organize the result
    n <- length(params$ind.vars)
    res.names <- names(res)
    rst <- list()
    r.coef <- arraydb.to.arrayr(res$coef, "double", n)
    r.std_err <- arraydb.to.arrayr(res$std_err, "double", n)
    r.z_stats <- arraydb.to.arrayr(res$z_stats, "double", n)
    r.p_values <- arraydb.to.arrayr(res$p_values, "double", n)
    n.grps <- dim(r.coef)[1] # how many groups
    r.odds_ratios <- arraydb.to.arrayr(res$odds_ratios, "double", n)
    r.ind.str <- params$ind.str
    r.grp.cols <- gsub("\"", "", arraydb.to.arrayr(params$grp.str,
                                                     "character", n))
    r.grp.expr <- params$grp.expr
    r.has.intercept <- params$has.intercept # do we have an intercept
    ## r.ind.vars <- gsub("\"", "", params$ind.vars)
    r.ind.vars <- params$ind.vars
    r.origin.ind <- params$origin.ind
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- call # the current function call itself
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr
    term.names <- .term.names(r.has.intercept, r.ind.vars, r.col.name, r.appear)

    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (j in seq(res.names))
            rst[[i]][[res.names[j]]] <- res[[res.names[j]]][[i]]
        rst[[i]]$coef <- r.coef[i,]
        if (all(is.na(rst[[i]]$coef))) {
            warning("NA in the result !")
            class(rst[[i]]) <- "logregr.madlib"
            next
        }
        names(rst[[i]]$coef) <- term.names
        rst[[i]]$std_err <- r.std_err[i,]
        names(rst[[i]]$std_err) <- term.names
        rst[[i]]$z_stats <- r.z_stats[i,]
        names(rst[[i]]$z_stats) <- term.names
        rst[[i]]$p_values <- r.p_values[i,]
        names(rst[[i]]$p_values) <- term.names
        rst[[i]]$odds_ratios <- r.odds_ratios[i,]
        rst[[i]]$grp.cols <- r.grp.cols
        rst[[i]]$grp.expr <- r.grp.expr
        rst[[i]]$has.intercept <- r.has.intercept
        rst[[i]]$ind.vars <- r.ind.vars
        rst[[i]]$origin.ind <- r.origin.ind
        rst[[i]]$ind.str <- r.ind.str
        rst[[i]]$col.name <- r.col.name
        rst[[i]]$appear <- r.appear
        rst[[i]]$call <- r.call
        rst[[i]]$dummy <- r.dummy
        rst[[i]]$dummy.expr <- r.dummy.expr
        rst[[i]]$model <- model
        rst[[i]]$terms <- params$terms
        rst[[i]]$factor.ref <- data@.factor.ref
        rst[[i]]$na.action <- na.action

        if (length(r.grp.cols) != 0) {
            ## cond <- Reduce(function(l, r) l & r,
            cond <- .row.action(.combine.list(Map(function(x) {
                if (is.na(rst[[i]][[r.grp.cols[x]]]))
                    ## is.na(origin.data[,x])
                    eval(parse(text = paste("with(origin.data, is.na(",
                               r.grp.expr[x], "))", sep = "")))
                else
                    ## origin.data[,x] == rst[[i]][[x]]
                    if (is.character(rst[[i]][[r.grp.cols[x]]]))
                        use <- "\"" %+% rst[[i]][[r.grp.cols[x]]] %+% "\""
                    else
                        use <- rst[[i]][[r.grp.cols[x]]]
                eval(parse(text = paste("with(origin.data, (",
                           r.grp.expr[x], ") ==",
                           use, ")", sep = "")))
            }, seq_len(length(r.grp.expr)))), " and ")
            rst[[i]]$data <- origin.data[cond,]
        } else
            rst[[i]]$data <- origin.data

        rst[[i]]$origin.data <- origin.data
        rst[[i]]$nobs <- nrow(rst[[i]]$data)

        class(rst[[i]]) <- "logregr.madlib"
    }

    class(rst) <- "logregr.madlib.grps" # use this to track summary

    if (n.grps == 1) return (rst[[1]])
    else return (rst)
}

## -----------------------------------------------------------------------

summary.logregr.madlib <- function (object, ...)
{
    object
}

summary.logregr.madlib.grps <- function (object, ...)
{
    object
}

## -----------------------------------------------------------------------

## Pretty format of linear regression result
print.logregr.madlib.grps <- function (x,
                                       digits = max(3L,
                                       getOption("digits") - 3L),
                                       ...)
{
    n.grps <- length(x)

    i <- 1
    while (i <= n.grps) if (!all(is.na(x[[i]]$coef))) break
    if (i == n.grps + 1) stop("All models' coefficients are NAs!")

    if (x[[i]]$has.intercept)
        rows <- c("(Intercept)", x[[i]]$ind.vars)
    else
        rows <- x[[i]]$ind.vars
    rows <- gsub("\"", "", rows)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)
    for (j in seq_len(length(x[[i]]$col.name)))
        if (x[[i]]$col.name[j] != x[[i]]$appear[j])
            rows <- gsub(x[[i]]$col.name[j], x[[i]]$appear[j], rows)
    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    rows <- gsub("\\s", "", rows)
    ind.width <- .max.width(rows)

    cat("\nMADlib Logistic Regression Result\n")
    cat("\nCall:\n", paste(deparse(x[[i]]$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    if (n.grps > 1)
        cat("\nThe data is divided into", x$grps, "groups\n")
    for (i in seq_len(n.grps))
    {
        cat("\n---------------------------------------\n\n")
        if (length(x[[i]]$grp.cols) != 0)
        {
            cat("Group", i, "when\n")
            for (col in seq_len(length(x[[i]]$grp.expr)))
                cat(x[[i]]$grp.expr[col], ": ",
                    x[[i]][[x[[i]]$grp.cols[col]]], "\n", sep = "")
            cat("\n")
        }

        cat("Coefficients:\n")
        printCoefmat(data.frame(cbind(Estimate = x[[i]]$coef,
                                      `Std. Error` = x[[i]]$std_err,
                                      `z value` = x[[i]]$z_stats,
                                      `Pr(>|z|)` = x[[i]]$p_values),
                                row.names = rows, check.names = FALSE),
                     digits = digits, signif.stars = TRUE)

        cat("Log likelihood:", x[[i]]$log_likelihood, "\n")
        cat("Condition Number:", x[[i]]$condition_no, "\n")
        cat("Number of iterations:", x[[i]]$num_iterations, "\n")
    }

    cat("\n")
}

## -----------------------------------------------------------------------

show.logregr.madlib.grps <- function (object)
{
    print(object)
}

## -----------------------------------------------------------------------

## Pretty format of linear regression result
print.logregr.madlib <- function (x,
                                  digits = max(3L,
                                  getOption("digits") - 3L),
                                  ...)
{
    if (all(is.na(x$coef))) stop("Coefficients are all NAs!")
    if (x$has.intercept)
        rows <- c("(Intercept)", x$ind.vars)
    else
        rows <- x$ind.vars
    rows <- gsub("\"", "", rows)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)
    for (i in seq_len(length(x$col.name)))
        if (x$col.name[i] != x$appear[i])
            rows <- gsub(x$col.name[i], x$appear[i], rows)
    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    rows <- gsub("\\s", "", rows)
    ind.width <- .max.width(rows)
    cat("\nMADlib Logistic Regression Result\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

    cat("\n---------------------------------------\n\n")
    if (length(x$grp.cols) != 0)
    {
        for (col in x$grp.cols)
            cat(col, ": ", x[[col]], ",\n", sep = "")
        cat("\n")
    }

    cat("Coefficients:\n")
    printCoefmat(data.frame(cbind(Estimate = x$coef,
                                  `Std. Error` = x$std_err,
                                  `z value` = x$z_stats,
                                  `Pr(>|z|)` = x$p_values),
                            row.names = rows, check.names = FALSE),
                 digits = digits, signif.stars = TRUE)

    cat("Log likelihood:", x$log_likelihood, "\n")
    cat("Condition Number:", x$condition_no, "\n")
    cat("Number of iterations:", x$num_iterations, "\n")

    cat("\n")
}

## -----------------------------------------------------------------------

show.logregr.madlib <- function (object)
{
    print(object)
}


## -----------------------------------------------------------------------

.madlib.mlogregr <- function (formula, data, na.action, method = "irls",
                              max.iter = 10000, tolerance = 1e-5, call)
{
    stop("To be implemented!")
}

## ------------------------------------------------------------

.madlib.glm <- function(formula, data, na.action = NULL, max.iter = 10000,
                        tolerance = 1e-5, family.name, link.name,
                        na.as.level = FALSE, verbose = FALSE, ...)
{
    if (!is(data, 'db.obj')) {
        stop('madlib.glm can only be used on a db.obj object, and ',
             deparse(substitute(data)), ' is not!')
    }
    origin.data <- data
    conn.id <- conn.id(data)

    ## Only MADlib 1.7 and newer are supported
    .check.madlib.version(data, allowed.version = 1.6)

    warnings <- .suppress.warnings(conn.id) # turn off warning messages

    analyzer <- .get.params(formula, data, na.action, na.as.level)
    data <- analyzer$data
    params <- analyzer$params
    is.tbl.source.temp <- analyzer$is.tbl.source.temp
    tbl.source <- content(data)

    madlib <- schema.madlib(conn.id) # Schema name for MADlib functions

    grp <- if (is.null(params$grp.str)) "NULL::text"
        else paste("'", params$grp.str, "'", sep = "")

    tbl.output <- .unique.string()

    sql <- paste("select ", madlib, ".glm('",
                 tbl.source, "', '",
                 tbl.output, "', '",
                 gsub("'", "''", params$dep.str), "', '",
                 params$ind.str, "', 'family=",
                 family.name, ", link=", link.name, "',",
                 grp, ", 'max_iter=", max.iter, ", optimizer=irls, tolerance=",
                 tolerance, "', ", verbose, ")", sep = "")

    res <- .db(sql, "; select * from ", tbl.output, nrows = -1,
               conn.id = conn.id, verbose = FALSE)

    if (is.tbl.source.temp) delete(tbl.source, conn.id)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)

    .restore.warnings(warnings) # turn on warning messages

    ## Organize the result ---------------------------------------------
    n <- length(params$ind.vars)
    res.names <- names(res)
    rst <- list()
    r.coef <- arraydb.to.arrayr(res$coef, 'double', n)
    r.std_err <- arraydb.to.arrayr(res$std_err, 'double', n)
    stats <- grepl("^(t|z)_stats$", res.names)
    r.stats <- arraydb.to.arrayr(res[, stats], 'double', n)
    r.p_values <- arraydb.to.arrayr(res$p_values, 'double', n)
    r.dispersion <- arraydb.to.arrayr(res$dispersion, 'double', n)
    n.grps <- dim(r.coef)[1] # how many groups
    r.ind.str <- params$ind.str
    r.grp.cols <- gsub("\"", "", arraydb.to.arrayr(params$grp.str, 'character', n))
    r.grp.expr <- params$grp.expr
    r.has.intercept <- params$has.intercept
    r.ind.vars <- params$ind.vars
    r.origin.ind <- params$origin.ind
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- call
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr
    term.names <- .term.names(r.has.intercept, r.ind.vars, r.col.name, r.appear)

    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (j in seq(res.names)) {
            rst[[i]][res.names[j]] <- res[[res.names[j]]][[i]]
        }
        rst[[i]]$coef <- r.coef[i, ]
        if (all(is.na(rst[[i]]$coef))) {
            warning('NA in the result !')
            class(rst[[i]]) <- 'glm.madlib'
            next
        }

        names(rst[[i]]$coef) <- term.names
        rst[[i]]$std_err <- r.std_err[i, ]
        names(rst[[i]]$std_err) <- term.names
        rst[[i]][[res.names[stats]]] <- r.stats[i,]
        names(rst[[i]][[res.names[stats]]]) <- term.names
        rst[[i]]$p_values <- r.p_values[i,]
        names(rst[[i]]$p_values) <- term.names
        rst[[i]]$dispersion <- r.dispersion[i]
        rst[[i]]$grp.cols <- r.grp.cols
        rst[[i]]$grp.expr <- r.grp.expr
        rst[[i]]$has.intercept <- r.has.intercept
        rst[[i]]$ind.vars <- r.ind.vars
        rst[[i]]$origin.ind <- r.origin.ind
        rst[[i]]$ind.str <- r.ind.str
        rst[[i]]$col.name <- r.col.name
        rst[[i]]$appear <- r.appear
        rst[[i]]$call <- r.call
        rst[[i]]$dummy <- r.dummy
        rst[[i]]$dummy.expr <- r.dummy.expr
        rst[[i]]$model <- model
        rst[[i]]$terms <- params$terms
        rst[[i]]$factor.ref <- data@.factor.ref
        rst[[i]]$na.action <- na.action
        rst[[i]]$family <- family.name
        rst[[i]]$link <- link.name

        if (length(r.grp.cols) != 0) {
            ## cond <- Reduce(function(l, r) l & r,
            cond <- .row.action(.combine.list(Map(function(x) {
                if (is.na(rst[[i]][[r.grp.cols[x]]]))
                    ## is.na(origin.data[,x])
                    eval(parse(text = paste("with(origin.data, is.na(",
                               r.grp.expr[x], "))", sep = "")))
                else
                    ## origin.data[,x] == rst[[i]][[x]]
                    if (is.character(rst[[i]][[r.grp.cols[x]]]))
                        use <- "\"" %+% rst[[i]][[r.grp.cols[x]]] %+% "\""
                    else
                        use <- rst[[i]][[r.grp.cols[x]]]
                eval(parse(text = paste("with(origin.data, (",
                           r.grp.expr[x], ") ==",
                           use, ")", sep = "")))
            }, seq_len(length(r.grp.expr)))), " and ")
            rst[[i]]$data <- origin.data[cond,]
        } else
            rst[[i]]$data <- origin.data

        rst[[i]]$origin.data <- origin.data
        rst[[i]]$nobs <- nrow(rst[[i]]$data)

        class(rst[[i]]) <- "glm.madlib"
    }

    class(rst) <- 'glm.madlib.grps'

    if (n.grps == 1) return (rst[[1]])
    else return (rst)
}

## ------------------------------------------------------------

## Print the GLM results

show.glm.madlib <- function(object)
{
    print(object)
}

show.glm.madlib.grps <- function(object)
{
    print(object)
}

summary.glm.madlib <- function(object, ...)
{
    object
}

summary.glm.madlib.grps <- function(object, ...)
{
    object
}

## ------------------------------------------------------------

.extract.rows <- function(x)
{
    if (x$has.intercept) {
        rows <- c("(Intercept)", x$ind.vars)
    } else {
        rows <- x$ind.vars
    }

    rows <- gsub("\"", "", rows)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)

    for (i in seq_len(length(x$col.name))) {
        if (x$col.name[i] != x$appear[i]) {
            rows <- gsub(x$col.name[i], x$appear[i], rows)
        }
    }

    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    gsub("\\s", "", rows)
}

## ------------------------------------------------------------

.print.coefs <- function(x, rows, digits)
{
    cat("Coefficients:\n")
    stats <- names(x)[grepl("^(t|z)_stats", names(x))]
    coef.fmat <- data.frame(cbind(x$coef, x$std_err, x[[stats]], x$p_values))
    stats <- if ('t_stats' %in% names(x)) "t value" else "z value"
    pvalue <- if ('t_stats' %in% names(x)) "Pr(>|t|)" else "Pr(>|z|)"
    names(coef.fmat) <- c("Estimate", "Std. Error", stats, pvalue)
    row.names(coef.fmat) <- rows

    printCoefmat(coef.fmat, digits = digits, signif.stars = TRUE)

    cat("Log likelihood:", x$log_likelihood, "\n")
    cat("Dispersion:", x$dispersion, "\n")
    cat("Number of iterations:", x$iteration, "\n")
}

## ------------------------------------------------------------

print.glm.madlib <- function(x,
                             digits = max(3L, getOption('digits') - 3L),
                             ...)
{
    if (all(is.na(x$coef))) stop("Coefficients are all NAs!")

    rows <- .extract.rows(x)

    cat("\nMADlib Generalized Linear Regression Result\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

    cat("\n ---------------------------------------\n\n")
    .print.coefs(x, rows, digits)

    cat("\n")
}

## ------------------------------------------------------------

print.glm.madlib.grps <- function(x,
                                  digits = max(3L, getOption('digits') - 3L),
                                  ...)
{
    n.grps <- length(x)

    i <- 1
    while (i <= n.grps) if (!all(is.na(x[[i]]$coef))) break
    if (i == n.grps + 1) stop("All models' coefficients are NAs!")

    rows <- .extract.rows(x[[i]])

    cat("\nMADlib Generalized Linear Regression Result\n")
    cat("\nCall:\n", paste(deparse(x[[i]]$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    if (n.grps > 1) {
        cat("\nThe data is divided into", x$grps, "groups\n")
    }

    for (i in seq_len(n.grps)) {
        cat("\n ---------------------------------------\n\n")
        if (length(x[[i]]$grp.cols) != 0) {
            cat("Group", i, "when\n")
            for (col in seq_len(length(x[[i]]$grp.expr)))
                cat(x[[i]]$grp.expr[col], ": ",
                    x[[i]][[x[[i]]$grp.cols[col]]], "\n", sep = "")
            cat("\n")
        }

        .print.coefs(x[[i]], rows, digits)
    }

    cat("\n")
}
