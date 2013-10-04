
## -----------------------------------------------------------------------
## Wrapper function for MADlib's linear, logistic and multinomial
## logistic regressions
## -----------------------------------------------------------------------

setClass("logregr.madlib")
setClass("logregr.madlib.grps")

## na.action is a place holder
## family specific parameters are in control, which
## is a list of parameters
madlib.glm <- function (formula, data, family = "gaussian",
                        na.action = "na.omit", control = list(), ...)
{
    args <- control
    args$formula <- formula
    args$data <- data
    args$na.action <- na.action
    call <- match.call()

    if (tolower(family) == "gaussian" || tolower(family) == "linear")
    {
        fit <- do.call(madlib.lm, args)
        if (is(fit, "lm.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    if (tolower(family) == "binomial" || tolower(family) == "logistic")
    {
        fit <- do.call(.madlib.logregr, args)
        if (is(fit, "logregr.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    if (family == "multinomial")
    {
        fit <- do.call(.madlib.mlogregr, args)
        if (is(fit, "mlogregr.madlib")) fit$call <- call
        else
            for (i in seq_len(length(fit))) fit[[i]]$call <- call
        return (fit)
    }

    cat("\nThe family", family, "is not supported!\n")
    return
}

## -----------------------------------------------------------------------

.madlib.logregr <- function (formula, data, na.action, method = "irls",
                             max_iter = 10000, tolerance = 1e-5)
{
    ## make sure fitting to db.obj
    if (! is(data, "db.obj"))
        stop("madlib.lm cannot be used on the object ",
             deparse(substitute(data)))

    origin.data <- data
    
    ## Only newer versions of MADlib are supported
    .check.madlib.version(data)

    warnings <- .suppress.warnings(conn.id(data))

    analyzer <- .get.params(formula, data)
    data <- analyzer$data
    params <- analyzer$params
    is.tbl.source.temp <- analyzer$is.tbl.source.temp
    tbl.source <- analyzer$tbl.source

    db.str <- (.get.dbms.str(conn.id(data)))$db.str
    
    ## dependent, independent and grouping strings
    if (is.null(params$grp.str))
        grp <- "NULL::text"
    else
        if (db.str == "HAWQ")
            stop("Right now MADlib on HAWQ does not support grouping ",
                 "in logistic regression !")
        else
            grp <- paste("'", params$grp.str, "'")

    ## construct SQL string
    conn.id <- conn.id(data)
    tbl.source <- gsub("\"", "", content(data))
    madlib <- schema.madlib(conn.id) # MADlib schema name
    if (db.str == "HAWQ") {
        tbl.output <- NULL
        sql <- paste0("select (f).* from (select ", madlib,
                      ".logregr('", tbl.source, "', '", params$dep.str,
                      "', '", params$ind.str, "', ", max_iter,
                      ", '", method, "', ", tolerance, ") as f) s")
    } else {
        tbl.output <- .unique.string()
        sql <- paste0("select ", madlib, ".logregr_train('",
                      tbl.source, "', '", tbl.output, "', '",
                      params$dep.str, "', '", params$ind.str, "', ",
                      grp, ", ", max_iter, ", '", method, "', ",
                      tolerance, ")")
    }

    ## execute the logistic regression and get the result
    res <- .get.res(sql, tbl.output, conn.id)

    ## drop temporary tables
    ## if (!is.null(tbl.output)) .db.removeTable(tbl.output, conn.id)
    if (is.tbl.source.temp) .db.removeTable(tbl.source, conn.id)

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
    r.has.intercept <- params$has.intercept # do we have an intercept
    r.ind.vars <- gsub("\"", "", params$ind.vars)
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- call # the current function call itself
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr

    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (j in seq(res.names))
            rst[[i]][[res.names[j]]] <- res[[res.names[j]]][[i]]
        rst[[i]]$coef <- r.coef[i,]
        rst[[i]]$std_err <- r.std_err[i,]
        rst[[i]]$z_stats <- r.z_stats[i,]
        rst[[i]]$p_values <- r.p_values[i,]
        rst[[i]]$odds_ratios <- r.odds_ratios[i,]
        rst[[i]]$grp.cols <- r.grp.cols
        rst[[i]]$has.intercept <- r.has.intercept
        rst[[i]]$ind.vars <- r.ind.vars
        rst[[i]]$ind.str <- r.ind.str
        rst[[i]]$col.name <- r.col.name
        rst[[i]]$appear <- r.appear
        rst[[i]]$call <- r.call
        rst[[i]]$dummy <- r.dummy
        rst[[i]]$dummy.expr <- r.dummy.expr
        rst[[i]]$model <- model
        rst[[i]]$terms <- params$terms
        rst[[i]]$nobs <- nrow(data)
        rst[[i]]$data <- origin.data
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
    
    if (x[[1]]$has.intercept)
        rows <- c("(Intercept)", x[[1]]$ind.vars)
    else
        rows <- x[[1]]$ind.vars
    for (i in seq_len(length(x[[1]]$col.name)))
        if (x[[1]]$col.name[i] != x[[1]]$appear[i])
            rows <- gsub(x[[1]]$col.name[i], x[[1]]$appear[i], rows)
    ind.width <- .max.width(rows)

    cat("\nMADlib Logistic Regression Result\n")
    cat("\nCall:\n", paste(deparse(x[[1]]$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    if (n.grps > 1)
        cat("\nThe data is divided into", x$grps, "groups\n")
    for (i in seq_len(n.grps))
    {
        cat("\n---------------------------------------\n\n")
        if (length(x[[i]]$grp.cols) != 0)
        {
            cat("Group", i, "when\n")
            for (col in x[[i]]$grp.cols)
                cat(col, ": ", x[[i]][[col]], ",\n", sep = "")
            cat("\n")
        }

        cat("Coefficients:\n")
        coef <- format(x[[i]]$coef, digits = digits)
        std.err <- format(x[[i]]$std_err, digits = digits)
        z.stats <- format(x[[i]]$z_stats, digits = digits)
        odds.ratios <- format(x[[i]]$odds_ratios, digits = digits)

        stars <- rep("", length(x[[i]]$p_values))
        for (j in seq(length(x[[i]]$p_values))) {
            if (is.na(x[[i]]$p_values[j]) || is.nan(x[[i]]$p_values[j])) {
                stars[j] <- " "
                next
            }
            if (x[[i]]$p_values[j] < 0.001)
                stars[j] <- "***"
            else if (x[[i]]$p_values[j] < 0.01)
                stars[j] <- "**"
            else if (x[[i]]$p_values[j] < 0.05)
                stars[j] <- "*"
            else if (x[[i]]$p_values[j] < 0.1)
                stars[j] <- "."
            else
                stars[j] <- " "
        }

        p.values <- paste(format(x[[i]]$p_values, digits = digits),
                          stars)
        output <- data.frame(cbind(Estimate = coef,
                                   `Std. Error` = std.err,
                                   `z value` = z.stats,
                                   `Pr(>|t|)` = p.values,
                                   `Odds ratio` = odds.ratios),
                             row.names = rows, check.names = FALSE)
        print(format(output, justify = "left"))

        cat("---\n")
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
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
    if (x$has.intercept)
        rows <- c("(Intercept)", x$ind.vars)
    else
        rows <- x$ind.vars
    for (i in seq_len(length(x$col.name)))
        if (x$col.name[i] != x$appear[i])
            rows <- gsub(x$col.name[i], x$appear[i], rows)
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
    coef <- format(x$coef, digits = digits)
    std.err <- format(x$std_err, digits = digits)
    z.stats <- format(x$z_stats, digits = digits)
    odds.ratios <- format(x$odds_ratios, digits = digits)
    
    stars <- rep("", length(x$p_values))
    for (j in seq(length(x$p_values))) {
        if (is.na(x$p_values[j]) || is.nan(x$p_values[j])) {
            stars[j] <- " "
            next
        }
        if (x$p_values[j] < 0.001)
            stars[j] <- "***"
        else if (x$p_values[j] < 0.01)
            stars[j] <- "**"
        else if (x$p_values[j] < 0.05)
            stars[j] <- "*"
        else if (x$p_values[j] < 0.1)
            stars[j] <- "."
        else
            stars[j] <- " "
    }
    
    p.values <- paste(format(x$p_values, digits = digits),
                      stars)
    output <- data.frame(cbind(Estimate = coef,
                               `Std. Error` = std.err,
                               `z value` = z.stats,
                               `Pr(>|t|)` = p.values,
                               `Odds ratio` = odds.ratios),
                         row.names = rows, check.names = FALSE)
    print(format(output, justify = "left"))
    
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
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
                              max_iter = 10000, tolerance = 1e-5, call)
{
    stop("To be implemented!")
}
