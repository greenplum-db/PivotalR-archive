## ----------------------------------------------------------------------
## Wrapper function for MADlib's lm function
## ----------------------------------------------------------------------

setClass("lm.madlib")
setClass("lm.madlib.grps")

## na.action is a place holder
## will implement later in R (using temp table), or will implement
## in MADlib
madlib.lm <- function (formula, data, na.action = NULL,
                       hetero = FALSE, na.as.level = FALSE,
                       ...) # param name too long
{
    ## make sure fitting to db.obj
    if (! is(data, "db.obj"))
        stop("madlib.lm can only be used on a db.obj object, and ",
             deparse(substitute(data)), " is not!")

    origin.data <- data

    ## Only newer versions of MADlib are supported
    .check.madlib.version(data)

    conn.id <- conn.id(data) # connection ID

    ## Check for HAWQ
    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str) && hetero)
        stop("MADlib on HAWQ 1.1 does not support computing ",
             "heteroskedasticity in linear regression.")

    ## suppress both SQL and R warnings
    warnings <- .suppress.warnings(conn.id)

    ## analyze the formula
    analyzer <- .get.params(formula, data, na.action, na.as.level)

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
        if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str)) {
            stop("MADlib on HAWQ 1.1 does not support grouping ",
                 "in linear regression.")
        } else if (.madlib.version.number(conn.id) > 0.7)
            grp <- paste("'", params$grp.str, "'", sep = "")
        else {
            grp <- paste("'{", params$grp.str, "}'::text[]")
        }

    tmp <- eval(parse(text = paste("with(data, ",
                      params$origin.dep, ")", sep = "")))

    if (tmp@.col.data_type %in% c("boolean", "text", "varchar"))
        stop("The dependent variable type is not supported ",
             "in linear regression!")

    ## construct SQL string
    ## tbl.source <- gsub("\"", "", content(data))
    tbl.source <- content(data)

    if (.madlib.version.number(conn.id) <= 0.7) {
        tbl.source <- gsub("\"", "", tbl.source)
    }

    madlib <- schema.madlib(conn.id) # MADlib schema name
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str)) {
        tbl.output <- NULL
        sql <- paste("select (f).* from (select ", madlib, ".linregr(",
                     params$dep.str, ",", params$ind.str, ") as f from ",
                     tbl.source, ") s", sep = "")
    } else {
        tbl.output <- .unique.string()
        sql <- paste("select ", madlib, ".linregr_train('",
                     tbl.source, "', '", tbl.output, "', '",
                     params$dep.str, "', '", params$ind.str, "', ",
                     grp, ", ", hetero, ")", sep = "")
    }

    ## execute and get the result, error handling is taken care of
    res <- db.q(sql, "; select * from ", tbl.output, nrows = -1,
                conn.id = conn.id, verbose = FALSE)

    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        model <- NULL
    else
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
    r.grp.expr <- params$grp.expr
    r.has.intercept <- params$has.intercept # do we have an intercept
    ## r.ind.vars <- gsub("\"", "", params$ind.vars)
    r.ind.vars <- params$ind.vars
    r.origin.ind <- params$origin.ind
    r.ind.str <- params$ind.str
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- match.call() # the current function call itself
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr
    term.names <- .term.names(r.has.intercept, r.ind.vars, r.col.name, r.appear)

    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (j in seq(res.names))
            rst[[i]][[res.names[j]]] <- res[[res.names[j]]][i]
        rst[[i]]$coef <- r.coef[i,]
        if (all(is.na(rst[[i]]$coef))) {
            warning("NA in the result !")
            class(rst[[i]]) <- "lm.madlib"
            next
        }
        names(rst[[i]]$coef) <- term.names
        rst[[i]]$std_err <- r.std_err[i,]
        names(rst[[i]]$std_err) <- term.names
        rst[[i]]$t_stats <- r.t_stats[i,]
        names(rst[[i]]$t_stats) <- term.names
        rst[[i]]$p_values <- r.p_values[i,]
        names(rst[[i]]$p_values) <- term.names
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
                else {
                    ## origin.data[,x] == rst[[i]][[x]]
                    if (is.character(rst[[i]][[r.grp.cols[x]]]))
                        use <- "\"" %+% rst[[i]][[r.grp.cols[x]]] %+% "\""
                    else
                        use <- rst[[i]][[r.grp.cols[x]]]
                    eval(parse(text = paste("with(origin.data, (",
                               r.grp.expr[x], ") ==",
                               use, ")", sep = "")))
                }
            }, seq_len(length(r.grp.expr)))), " and ")
            rst[[i]]$data <- origin.data[cond,]
        } else
            rst[[i]]$data <- origin.data

        rst[[i]]$origin.data <- origin.data
        rst[[i]]$nobs <- nrow(rst[[i]]$data)

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
    if (is.tbl.source.temp) delete(tbl.source, conn.id)

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

    cat("\nMADlib Linear Regression Result\n")
    cat("\nCall:\n", paste(deparse(x[[i]]$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
    if (n.grps > 1)
        cat("\nThe data is divided into", n.grps, "groups\n")
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
                                      `t value` = x[[i]]$t_stats,
                                      `Pr(>|t|)` = x[[i]]$p_values),
                                row.names = rows, check.names = FALSE),
                 digits = digits, signif.stars = TRUE)
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
    if (all(is.na(x$coef))) stop("Coefficients are NAs!")
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
    printCoefmat(data.frame(cbind(Estimate = x$coef,
                                  `Std. Error` = x$std_err,
                                  `t value` = x$t_stats,
                                  `Pr(>|t|)` = x$p_values),
                            row.names = rows, check.names = FALSE),
                 digits = digits, signif.stars = TRUE)

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

## -----------------------------------------------------------------------

.term.names <- function(has.intercept, ind.vars, col.name, appear)
{
    if (has.intercept)
        rows <- c("(Intercept)", ind.vars)
    else
        rows <- ind.vars
    rows <- gsub("\"", "", rows)
    rows <- gsub("::[\\w\\s]+", "", rows, perl = T)
    for (i in seq_len(length(col.name)))
        if (col.name[i] != appear[i])
            rows <- gsub(col.name[i], appear[i], rows)
    rows <- gsub("\\(([^\\[\\]]*?)\\)\\[(\\d+?)\\]", "\\1[\\2]", rows)
    rows <- .reverse.consistent.func(rows)
    rows <- gsub("\\s", "", rows)
    rows
}
