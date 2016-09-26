
setClass("svm.madlib")
setClass("svm.madlib.grps")


madlib.svm <- function (formula, data,
                        na.action = NULL, na.as.level = FALSE,
                        type = c("classification", "regression", "one-class"),
                        kernel = c("gaussian", "linear", "polynomial"),
                        degree = 3, gamma = NULL, coef0 = 1.0, class.weight = NULL,
                        tolerance = 1e-10, epsilon = NULL, cross = 0, lambda = 0.01,
                        control = list(), verbose = FALSE, ...)
{
    if (!is(data, "db.obj"))
        stop("madlib.svm can only be used on a db.obj object, and ",
             deparse(substitute(data)), " is not!")

    kernel <- match.arg(kernel)
    type <- match.arg(type)
    if (!is.list(control))
        stop("control must be a list of parameters!")
    if (type == "one-class" && is.null(class.weight))
        class.weight <- "balanced"
    kernel.control <- .validate.kernel(kernel, degree, gamma, coef0, control)
    params.str <- .validate.params(class.weight, tolerance,
                                   epsilon, cross, lambda, control)

    kernel.str = kernel.control$func
    kernel.params.str <- kernel.control$params

    origin.data <- data
    conn.id <- conn.id(data)
    .check.madlib.version(data)

    db <- .get.dbms.str(conn.id)
    warnings <- .suppress.warnings(conn.id)

    analyzer <- .get.params(formula, data, na.action, na.as.level)
    data <- analyzer$data
    params <- analyzer$params
    is.tbl.source.temp <- analyzer$is.tbl.source.temp

    tbl.source <- content(data)
    madlib <- schema.madlib(conn.id)
    ind.str <- params$ind.str
    dep.str <- params$dep.str
    grp.str <- if (is.null(params$grp.str)) "" else params$grp.str
    tbl.output <- .unique.string()

    if (type == "one-class") {
        sql <- sprintf("select %s.svm_one_class('%s', '%s', '%s',
                       '%s', '%s', '%s', '%s', %s);", madlib,
                       tbl.source, tbl.output, ind.str, kernel.str,
                       kernel.params.str, grp.str, params.str, verbose)
    } else {
        sql <- sprintf("select %s.svm_%s('%s', '%s', '%s', '%s',
                       '%s', '%s', '%s', '%s', %s);", madlib, type,
                       tbl.source, tbl.output, dep.str, ind.str, kernel.str,
                       kernel.params.str, grp.str, params.str, verbose)
    }
    db.q(sql, conn.id = conn.id, verbose = FALSE)
    res.summary <- db.q(paste0("select * from ", tbl.output, "_summary"),
                        conn.id = conn.id, verbose = FALSE)
    res <- db.q(paste0("select * from ", tbl.output),
                conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    model.random <- NULL
    if (db.existsObject(paste0(tbl.output, "_random"), conn.id))
        model.random <- db.data.frame(paste0(tbl.output, "_random"),
                                      conn.id = conn.id, verbose = FALSE)

    .restore.warnings(warnings)

    rst <- list()
    res.names <- names(res)
    res.summary.names <- names(res.summary)
    n <- length(params$ind.vars)
    r.sql <- sql
    r.coef <- arraydb.to.arrayr(res$coef, "double", n)
    n.grps <- dim(r.coef)[1]

    r.grp.cols <- gsub("\"", "", arraydb.to.arrayr(params$grp.str,
                                                   "character", n))
    r.grp.expr <- params$grp.expr
    r.has.intercept <- params$has.intercept
    r.ind.vars <- params$ind.vars
    r.origin.ind <- params$origin.ind
    r.ind.str <- params$ind.str
    r.col.name <- gsub("\"", "", data@.col.name)
    r.appear <- data@.appear.name
    r.call <- match.call() # the current function call itself
    r.dummy <- data@.dummy
    r.dummy.expr <- data@.dummy.expr

    for (i in seq_len(n.grps)) {
        rst[[i]] <- list()
        for (name in res.names)
            rst[[i]][[name]] <- res[[name]][i]
        for (name in res.summary.names)
            rst[[i]][[name]] <- res.summary[[name]]
        rst[[i]]$coef <- r.coef[i,]
        if (all(is.na(rst[[i]]$coef))) {
            warning("NA in the result !")
            class(rst[[i]]) <- "svm.madlib"
            next
        }
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
        rst[[i]]$terms <- params$terms
        rst[[i]]$factor.ref <- data@.factor.ref
        rst[[i]]$na.action <- na.action
        rst[[i]]$sql <- r.sql
        rst[[i]]$grp.i <- i

        rst[[i]]$model <- model
        rst[[i]]$model.summary <- model.summary
        rst[[i]]$model.random <- model.random

        rst[[i]]$lambda <- lambda
        rst[[i]]$cross <- cross
        rst[[i]]$coef0 <- coef0
        rst[[i]]$degree <- degree
        # find gamma value in the kernel params string
        pattern <- "(?<=gamma=)(\\s*[0-9]+\\.?[0-9]*\\s*)"
        gamma <- as.numeric(regmatches(rst[[i]]$kernel_params,
                    regexpr(pattern, rst[[i]]$kernel_params, perl = TRUE)))
        rst[[i]]$gamma <- gamma
        rst[[i]]$epsilon <- if (is.null(epsilon)) 0.01 else epsilon

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

        class(rst[[i]]) <- "svm.madlib" # A single model class

    }

    if (is.tbl.source.temp) delete(tbl.source, conn.id)

    class(rst) <- "svm.madlib.grps"

    ## If no grouping, just return one model
    ## Otherwise, return a list of models
    if (n.grps == 1) return (rst[[1]])
    else return (rst)

}

## -----------------------------------------------------------------------

.validate.params <- function (class.weight, tolerance, epsilon, cross, lambda, control)
{
    params <- list(tolerance = tolerance,
                   lambda = lambda,
                   n.folds = cross)
    # default is NULL
    params$class.weight <- class.weight
    params$epsilon <- epsilon

    control.names <- names(control)
    all.params <- c("init.stepsize", "decay.factor", "max.iter", "norm",
                    "eps.table", "validation.result")
    wrong.params <- setdiff(control.names, all.params)
    if (!identical(wrong.params, character(0)) && !is.null(wrong.params))
        stop("Some of the control parameters are not supported: ",
             paste(wrong.params, seq="", collapse=", "))
    for (param.name in control.names)
        params[[param.name]] <- control[[param.name]]
    for (param.name in names(params)) {
        param = params[[param.name]]
        if (is.list(param))
            stop("list is not allowed in control parameters!")
        if (is.numeric(param) && length(param) > 1)
            params[[param.name]] <- paste("[", paste(param, collapse = ", "),
                                            "]", sep = "")
    }
    nms <- gsub("\\.", "_", names(params))
    paste(nms, " = ", as.character(params), sep = "", collapse = ", ")
}

## ----------------------------------------------------------------------

.validate.kernel <- function (kernel, degree, gamma, coef0, control)
{
    if (!is.character(kernel))
        stop("kernel must be \"linear\", \"gaussian\" or \"polynomial\"")
    kernel <- tolower(kernel)
    if (! kernel %in% c("gaussian", "linear", "polynomial"))
        stop("kernel must be \"linear\", \"gaussian\" or \"polynomial\"")
    kernel.params = list()
    if ("n.components" %in% names(control)) {
        kernel.params$n.components <- control$n.components
    }
    if ("fit.intercept" %in% names(control)) {
        kernel.params$fit.intercept <- control$fit.intercept
    }
    if (kernel == "gaussian") {
        kernel.params$gamma <- gamma
    }
    else if (kernel == "polynomial") {
        kernel.params$degree <- degree
        kernel.params$coef0 <- coef0
    }
    nms <- names(kernel.params)
    nms <- gsub("\\.", "_", nms)
    list(func = kernel,
         params = if (is.null(nms) || identical(nms, character(0))) ""
         else paste(nms, " = ", as.character(kernel.params),
                    sep = "", collapse = ", "))
}

## -----------------------------------------------------------------------

## Print a single model
print.svm.madlib <- function (x, ...)
{
    if (all(is.na(x$coef))) stop("Coefficients are NAs!")
    cat("\nMADlib", x$method, "Result\n")
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

    cat("\n---------------------------------------\n\n")
    .print.params.svm.madlib(x, x$grp.i)
}

## -----------------------------------------------------------------------

summary.svm.madlib <- function (object, ...)
{
    object
}

summary.svm.madlib.grps <- function (object, ...)
{
    object
}

show.svm.madlib <- function (object)
{
    print(object)
}

show.svm.madlib.grps <- function (object)
{
    print(object)
}

## -----------------------------------------------------------------------

## Pretty format of svm result
## Print a list of models
print.svm.madlib.grps <- function (x, ...)
{
    n.grps <- length(x)

    i <- 1
    while (i <= n.grps) if (!all(is.na(x[[i]]$coef))) break
    if (i == n.grps + 1) stop("All models' coefficients are NAs!")

    cat("\nMADlib", x[[i]]$method, "Result\n")
    cat("\nCall:\n", paste(deparse(x[[i]]$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

    if (n.grps > 1)
        cat("\nThe data is divided into", n.grps, "groups\n")

    for (i in seq_len(n.grps)) {
        cat("\n---------------------------------------\n\n")
        .print.params.svm.madlib(x[[i]], i)
    }
}

## -----------------------------------------------------------------------

.print.params.svm.madlib <- function (x, grp = 1, ...)
{
    if (length(x$grp.cols) != 0)
    {
        cat("Group", grp, "when\n")
        for (i in seq_len(length(x$grp.cols))) {
            col <- x$grp.cols[[i]]
            expr <- x$grp.expr[[i]]
            cat(col, " (", expr, ") ", ": ", x[[col]], "\n", sep = "")
        }
        cat("\n")
    }

    format.str <- "%12s:"
    cat("Objective\n")
    cat("=========\n")
    cat(sprintf(format.str, "hinge-loss"), x$loss, "\n")
    cat(sprintf(format.str, "|gradient|"), x$norm_of_gradient, "\n")

    cat("\n")

    cat("Parameters\n")
    cat("==========\n")
    cat(sprintf(format.str, "SVM-Type"), x$method, "\n")
    cat(sprintf(format.str, "SVM-Kernel"), x$kernel_func, "\n")
    cat(sprintf(format.str, "lambda"), x$lambda, "\n")
    if (x$method == "SVR")
        cat(sprintf(format.str, "epsilon"), x$epsilon, "\n")
    if (x$kernel_func == "gaussian") {
        cat(sprintf(format.str, "gamma"), x$gamma, "\n")
    } else if (x$kernel_func == "polynomial") {
        cat(sprintf(format.str, "degree"), x$degree, "\n")
        cat(sprintf(format.str, "coef0"), x$coef0, "\n")
    }

    cat("\n")
}

## -----------------------------------------------------------------------

predict.svm.madlib.grps <- function (object, newdata, id.col, ...)
{
    n.grps <- length(object)

    i <- 1
    while (i <= n.grps) if (!all(is.na(object[[i]]$coef))) break
    if (i == n.grps + 1) stop("All models' coefficients are NAs!")

    x <- object[[i]]

    predict(x, newdata, id.col)
}

## -----------------------------------------------------------------------

# Object parameter is the output of the training function
predict.svm.madlib <- function (object, newdata, id.col, ...)
{
    conn.id <- conn.id(newdata)
    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id)


    tbl.source <- content(newdata)
    tbl.output <- .unique.string()
    tbl.model <- content(object$model)

    sql <- paste("select ", madlib, ".svm_predict('", tbl.model,
                "', '", tbl.source, "', '", id.col, "', '", tbl.output, "')", sep = "")

    .db(sql, conn.id = conn.id, verbose = FALSE)
    db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
}
