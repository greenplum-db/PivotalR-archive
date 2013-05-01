
## ------------------------------------------------------------------------
## Wrapper function for MADlib's linear, logistic and multinomial
## logistic regressions
## ------------------------------------------------------------------------

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
    call <- deparse(match.call())
    
    if (tolower(family) == "gaussian" || tolower(family) == "linear")
    {
        fit <- do.call(madlib.lm, args)
        fit$call <- call
        return (fit)
    }

    if (tolower(family) == "binomial" || tolower(family) == "logistic")
    {
        fit <- do.call(.madlib.logregr, args)
        fit$call <- call
        return (fit)
    }

    if (family == "multinomial")
    {
        fit <- do.call(.madlib.mlogregr, args)
        fit$call <- call
        return (fit)
    }

    cat("\nThe family", family, "is not supported!\n")
    return
}

## ------------------------------------------------------------------------

.madlib.logregr <- function (formula, data, na.action, method = "irls",
                             max_iter = 10000, tolerance = 1e-5)
{
    ## make sure fitting to db.obj
    if (! is(data, "db.obj"))
        stop("madlib.lm cannot be used on the object ",
             deparse(substitute(data)))

    msg.level <- .set.msg.level("panic") # suppress all messages
    ## disable warning in R, RPostgreSQL
    ## prints some unnessary warning messages
    warn.r <- getOption("warn")
    options(warn = -1)
    
    ## create temp table for db.Rquery objects
    is.tbl.source.temp <- FALSE
    if (is(data, "db.Rquery"))
    {
        tbl.source <- .unique.string()
        is.tbl.source.temp <- TRUE
        data <- as.db.data.frame(data, tbl.source, is.temp = TRUE)
    }

    ## dependent, independent and grouping strings
    params <- .analyze.formula(formula, data)
    if (is.null(params$grp.str))
        grp <- "NULL::text"
    else
        grp <- paste("'", params$grp.str, "'")

    ## construct SQL string
    conn.id <- conn.id(data)
    tbl.source <- content(data)
    tbl.output <- .unique.string()
    madlib <- schema.madlib(conn.id) # MADlib schema name
    sql <- paste("select ", madlib, ".logregr_train('",
                 tbl.source, "', '", tbl.output, "', '",
                 params$dep.str, "', '", params$ind.str, "', ",
                 grp, ", ", max_iter, ", '", method, "', ",
                 tolerance, ")", sep = "")

    ## execute the linear regression
    res <- try(.db.getQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class))
        stop("Could not run MADlib logistic regression !")

    ## retreive result
    res <- try(.db.getQuery(paste("select * from", tbl.output), conn.id),
               silent = TRUE)
    if (is(res, .err.class))
        stop("Could not retreive MADlib linear regression result !")

    ## drop temporary tables
    .db.removeTable(tbl.output, conn.id)
    if (is.tbl.source.temp) .db.removeTable(tbl.source, conn.id)
    
    msg.level <- .set.msg.level(msg.level) # reset message level
    options(warn = warn.r) # reset R warning level
    
    ## organize the result
    rst <- list()
    res.names <- names(res)
    for (i in seq(res.names))
        rst[[res.names[i]]] <- res[[res.names[i]]]
    rst$coef <- arraydb.to.arrayr(res$coef, "double")
    rst$std_err <- arraydb.to.arrayr(res$std_err, "double")
    rst$z_stats <- arraydb.to.arrayr(res$z_stats, "double")
    rst$p_values <- arraydb.to.arrayr(res$p_values, "double")
    rst$odds_ratios <- arraydb.to.arrayr(res$odds_ratios, "double")

    ## other useful information
    rst$grps <- dim(rst$coef)[1] # how many groups
    rst$grp.cols <- arraydb.to.arrayr(params$grp.str, "character")
    rst$has.intercept <- params$has.intercept # do we have an intercept
    rst$ind.vars <- params$ind.vars
    rst$call <- call # the current function call itself

    class(rst) <- "logregr.madlib" # use this to track summary
    rst
}

## ------------------------------------------------------------------------

summary.logregr.madlib <- function (object, ...)
{
    object
}

## ------------------------------------------------------------------------

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
    ind.width <- .max.width(rows)

    cat("\nMADlib Logistic Regression Result\n")
    cat("\nCall:\n", paste(x$call, sep = "\n", collapse = "\n"),
        "\n", sep = "")
    if (x$grps > 1)
        cat("\nThe data is divided into", x$grps, "groups\n")
    for (i in seq_len(x$grps))
    {
        cat("\n---------------------------------------\n\n")
        if (! is.null(x$grp.cols))
        {
            cat("When\n")
            for (col in x$grp.cols)
                cat(col, ": ", x[[col]][i], ",\n", sep = "")
            cat("\n")
        }

        cat("Coefficients:\n")
        coef <- format(x$coef[i,], digits = digits)
        std.err <- format(x$std_err[i,], digits = digits)
        z.stats <- format(x$z_stats[i,], digits = digits)
        odds.ratios <- format(x$odds_ratios[i,], digits = digits)
        
        stars <- rep("", length(x$p_values[i,]))
        for (j in seq(x$p_values[i,]))
            if (x$p_values[i,j] < 0.001)
                stars[j] <- "***"
            else if (x$p_values[i,j] < 0.01)
                stars[j] <- "**"
            else if (x$p_values[i,j] < 0.05)
                stars[j] <- "*"
            else if (x$p_values[i,j] < 0.1)
                stars[j] <- "."
            else
                stars[j] <- " "
        p.values <- paste(format(x$p_values[i,], digits = digits),
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
        cat("Log likelihood:", x$log_likelihood[i], "\n")
        cat("Condition Number:", x$condition_no[i], "\n")
        cat("Number of iterations:", x$num_iterations[i], "\n")
    }

    cat("\n")
}

## ------------------------------------------------------------------------

show.logregr.madlib <- function (object)
{
    print(object)
}

## ------------------------------------------------------------------------

.madlib.mlogregr <- function (formula, data, na.action, method = "irls",
                              max_iter = 10000, tolerance = 1e-5, call)
{
    stop("To be implemented!")
}
