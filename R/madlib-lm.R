
## ------------------------------------------------------------------------
## Wrapper function for MADlib's lm function
## ------------------------------------------------------------------------

## na.action is a place holder
## will implement later in R (using temp table), or will implement
## in MADlib
madlib.lm <- function (formula, data, na.action, 
                       hetero = FALSE, ...) # param name too long
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
        grp <- "NULL::text[]"
    else
        grp <- paste("'{", params$grp.str, "}'::text[]")

    ## construct SQL string
    conn.id <- conn.id(data)
    tbl.source <- content(data)
    tbl.output <- .unique.string()
    madlib <- schema.madlib(conn.id) # MADlib schema name
    sql <- paste("select ", madlib, ".linregr_train('",
                 tbl.source, "', '", tbl.output, "', '",
                 params$dep.str, "', '", params$ind.str, "', ",
                 grp, ", ", hetero, ")", sep = "")

    ## execute the linear regression
    res <- try(.db.getQuery(sql, conn.id), silent = TRUE)
    if (is(res, .err.class))
        stop("Could not run MADlib linear regression !")

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
    rst$t_stats <- arraydb.to.arrayr(res$t_stats, "double")
    rst$p_values <- arraydb.to.arrayr(res$p_values, "double")

    ## other useful information
    rst$grps <- dim(rst$coef)[1] # how many groups
    rst$grp.cols <- arraydb.to.arrayr(params$grp.str, "character")
    rst$has.intercept <- params$has.intercept # do we have an intercept
    rst$ind.vars <- params$ind.vars
    rst$call <- deparse(match.call()) # the current function call itself
    
    class(rst) <- "lm.madlib" # use this to track summary
    rst
}

## ------------------------------------------------------------------------

summary.lm.madlib <- function (object, ...)
{
    object
}

## ------------------------------------------------------------------------

## Pretty format of linear regression result
print.lm.madlib <- function (x,
                             digits = max(3L, getOption("digits") - 3L),
                             ...)
{
    if (x$has.intercept)
        rows <- c("(Intercept)", x$ind.vars)
    else
        rows <- x$ind.vars
    ind.width <- .max.width(rows)

    cat("\nMADlib Linear Regression Result\n")
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
                cat(col, ": ", x[[col]][i], "\n\n", sep = "")
            cat("We have\n")
        }

        cat("Coefficients:\n")
        coef <- format(x$coef[i,], digits = digits)
        std.err <- format(x$std_err[i,], digits = digits)
        t.stats <- format(x$t_stats[i,], digits = digits)

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
                                   `t value` = t.stats,
                                   `Pr(>|t|)` = p.values),
                             row.names = rows, check.names = FALSE)
        print(format(output, justify = "left"))

        cat("---\n")
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
        cat("R-squared:", x$r2[i], "\n")
        cat("Condition Number:", x$condition_no[i], "\n")

        if (!is.null(x$bp_stats))
        {
            cat("Breusch-Pagan test statistics:", x$bp_stats[i], "\n")
            cat("Breusch-Pagan test p-value:", x$bp_p_value[i], "\n")
        }        
    }

    cat("\n")
}

## ------------------------------------------------------------------------

show.lm.madlib <- function (object)
{
    print(object)
}
