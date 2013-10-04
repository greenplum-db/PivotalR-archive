
## ----------------------------------------------------------------------
## Wrapper function for MADlib's lm function
## ----------------------------------------------------------------------

setClass("lm.madlib")
setClass("lm.madlib.grps")

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

    origin.data <- data
    
    ## Only newer versions of MADlib are supported
    .check.madlib.version(data)

    conn.id <- conn.id(data) # connection ID

    ## Check for HAWQ
    db.str <- (.get.dbms.str(conn.id))$db.str
    if (db.str == "HAWQ" && hetero)
        stop("Currently MADlib on HAWQ does not support computing ",
             "heteroskedasticity in linear regression.")

    ## suppress both SQL and R warnings
    warnings <- .suppress.warnings(conn.id)

    ## analyze the formula
    analyzer <- .get.params(formula, data)

    ## For db.view or db.R.query, create a temporary table
    ## For pivoted db.Rquery, realize the pivoting
    ## Else: just return the original data table
    data <- analyzer$data

    ## dependent, independent and grouping variables
    ## has.intercept boolean
    params <- analyzer$params

    ## Is data temporarily created?
    is.tbl.source.temp <- analyzer$is.tbl.source.temp
    
    ## grouping string
    if (is.null(params$grp.str))
        grp <- "NULL"
    else
        if (db.str == "HAWQ") {
            stop("Currently MADlib on HAWQ does not support grouping ",
                 "in linear regression.")
        } else if (.madlib.version.number(conn.id) > 0.7)
            grp <- paste0("'", params$grp.str, "'")
        else
            grp <- paste("'{", params$grp.str, "}'::text[]")

    ## construct SQL string
    tbl.source <- gsub("\"", "", content(data))
    madlib <- schema.madlib(conn.id) # MADlib schema name
    if (db.str == "HAWQ") {
        tbl.output <- NULL
        sql <- paste0("select (f).* from (select ", madlib, ".linregr(",
                      params$dep.str, ",", params$ind.str, ") as f from ",
                      tbl.source, ") s")
    } else {
        tbl.output <- .unique.string()
        sql <- paste0("select ", madlib, ".linregr_train('",
                      tbl.source, "', '", tbl.output, "', '",
                      params$dep.str, "', '", params$ind.str, "', ",
                      grp, ", ", hetero, ")")
    }
        
    ## execute and get the result, error handling is taken care of
    res <- .get.res(sql, tbl.output, conn.id)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)

    ## reset SQL and R warning levels
    .restore.warnings(warnings)

    ## organize the result (into groups)
    n <- length(params$ind.vars)
    res.names <- names(res)
    rst <- list()
    r.coef <- arraydb.to.arrayr(res$coef, "double", n)
    r.std_err <- arraydb.to.arrayr(res$std_err, "double", n)
    r.t_stats <- arraydb.to.arrayr(res$t_stats, "double", n)
    r.p_values <- arraydb.to.arrayr(res$p_values, "double", n)
    n.grps <- dim(r.coef)[1] # how many groups
    r.grp.cols <- gsub("\"", "", arraydb.to.arrayr(params$grp.str,
                                                   "character", n))
    r.has.intercept <- params$has.intercept # do we have an intercept
    r.ind.vars <- gsub("\"", "", params$ind.vars)
    r.ind.str <- params$ind.str
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- match.call() # the current function call itself
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr
    
    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (j in seq(res.names))
            rst[[i]][[res.names[j]]] <- res[[res.names[j]]][i]
        rst[[i]]$coef <- r.coef[i,]
        rst[[i]]$std_err <- r.std_err[i,]
        rst[[i]]$t_stats <- r.t_stats[i,]
        rst[[i]]$p_values <- r.p_values[i,]
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
        class(rst[[i]]) <- "lm.madlib" # A single model class

        ## get error SS manually using predicted values
        ## This takes too much time. Move it into isolated functions
        ## pred <- .predict(rst[[i]], data, "linregr_predict", "double precision", "float8")
        ## y <- eval(params$terms[[2]], as.environment(data))
        ## rst[[i]]$sse <- lookat(sum((y - pred)^2))[1, 1, drop=TRUE]
    }

    ## drop temporary tables
    ## HAWQ does not need to drop the output table
    ## if (!is.null(tbl.output)) .db.removeTable(tbl.output, conn.id)
    if (is.tbl.source.temp) .db.removeTable(tbl.source, conn.id)

    ## the class of a list of models
    class(rst) <- "lm.madlib.grps"

    ## If no grouping, just return one model
    ## Otherwise, return a list of models
    if (n.grps == 1) return (rst[[1]])
    else return (rst)
}

## -----------------------------------------------------------------------

summary.lm.madlib <- function (object, ...)
{
    object
}

summary.lm.madlib.grps <- function (object, ...)
{
    object
}


## -----------------------------------------------------------------------

## Pretty format of linear regression result
## Print a list of models
## NOTE: code needs refactoring
print.lm.madlib.grps <- function (x,
                                  digits = max(3L, getOption("digits") - 3L),
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

    cat("\nMADlib Linear Regression Result\n")
    cat("\nCall:\n", paste(deparse(x[[1]]$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    if (n.grps > 1)
        cat("\nThe data is divided into", n.grps, "groups\n")
    for (i in seq_len(n.grps))
    {
        cat("\n---------------------------------------\n\n")
        if (length(x[[i]]$grp.cols) != 0)
        {
            cat("Group", i, "when\n")
            for (col in x[[i]]$grp.cols)
                cat(col, ": ", x[[i]][[col]], "\n", sep = "")
            cat("\n")
        }

        cat("Coefficients:\n")
        coef <- format(x[[i]]$coef, digits = digits)
        std.err <- format(x[[i]]$std_err, digits = digits)
        t.stats <- format(x[[i]]$t_stats, digits = digits)

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
                                   `t value` = t.stats,
                                   `Pr(>|t|)` = p.values),
                             row.names = rows, check.names = FALSE)
        print(format(output, justify = "left"))

        cat("---\n")
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
        cat("R-squared:", x[[i]]$r2, "\n")
        cat("Condition Number:", x[[i]]$condition_no, "\n")

        if (!is.null(x[[i]]$bp_stats))
        {
            cat("Breusch-Pagan test statistics:", x[[i]]$bp_stats, "\n")
            cat("Breusch-Pagan test p-value:", x[[i]]$bp_p_value, "\n")
        }        
    }

    cat("\n")
}

## -----------------------------------------------------------------------

show.lm.madlib.grps <- function (object)
{
    print(object)
}

## -----------------------------------------------------------------------

## Print a single model
## NOTE: Code needs refactoring
print.lm.madlib <- function (x,
                             digits = max(3L, getOption("digits") - 3L),
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

    cat("\nMADlib Linear Regression Result\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    
    cat("\n---------------------------------------\n\n")
    if (length(x$grp.cols) != 0)
    {
        for (col in x$grp.cols)
            cat(col, ": ", x[[col]], "\n", sep = "")
        cat("\n")
    }
    
    cat("Coefficients:\n")
    coef <- format(x$coef, digits = digits)
    std.err <- format(x$std_err, digits = digits)
    t.stats <- format(x$t_stats, digits = digits)
    
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
                               `t value` = t.stats,
                               `Pr(>|t|)` = p.values),
                         row.names = rows, check.names = FALSE)
    print(format(output, justify = "left"))
    
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
    cat("R-squared:", x$r2, "\n")
    cat("Condition Number:", x$condition_no, "\n")
    
    if (!is.null(x$bp_stats))
    {
        cat("Breusch-Pagan test statistics:", x$bp_stats, "\n")
        cat("Breusch-Pagan test p-value:", x$bp_p_value, "\n")
    }        
    
    cat("\n")
}

## -----------------------------------------------------------------------

show.lm.madlib <- function (object)
{
    print(object)
}
