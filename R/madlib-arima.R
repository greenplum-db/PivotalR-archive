
## ----------------------------------------------------------------------
## Wrapper function for MADlib's ARIMA
## ----------------------------------------------------------------------

setGeneric ("madlib.arima",
            def = function (x, ts, ...) {
                call <- match.call()
                fit <- standardGeneric("madlib.arima")
                fit$call <- call # call must be in generic function
                fit
            })

setClass("arima.css.madlib")

## ----------------------------------------------------------------------

## One of the two interfaces of madlib.arima
setMethod (
    "madlib.arima",
    signature (x = "db.Rquery", ts = "db.Rquery"),
    def = function (x, ts, by = NULL, order=c(1,1,1),
    seasonal = list(order = c(0,0,0), period = NA),
    include.mean = TRUE, method = "CSS",
    optim.method = "LM",
    optim.control = list(), ...)
{
    if (length(names(x)) != 1 || length(names(ts)) != 1)
        stop("ARIMA can only have one time stamp column and ",
             "one time series value column !")
    
    data <- cbind(x, ts)
    f.str <- paste(names(x), "~", names(ts))

    ## grouping is a list of db.Rquery
    if (!is.null(by)) {
        grp.names <- character(0)
        for (i in seq_len(length(by))) {
            data <- cbind(data, by[[i]])
            grp.names <- c(grp.names, names(by[[i]]))
        }
        f.str <- paste(f.str, "|", paste(grp.names, collapse = "+"))
    }

    ## Transform to the other interface
    madlib.arima(formula(f.str), data, order, seasonal, include.mean,
                 method, optim.method, optim.control, ...)
})

## ----------------------------------------------------------------------

setMethod (
    "madlib.arima",
    signature (x = "formula", ts = "db.obj"),
    def = function (x, ts, order=c(1,1,1),
    seasonal = list(order = c(0,0,0), period = NA),
    include.mean = TRUE, method = "CSS",
    optim.method = "LM",
    optim.control = list(), ...)
{
    ## Prepare for another function
    ## To avoid argument checking/matching
    args <- list(...)
    args <- c(args, optim.control)
    args$formula <- x
    args$data <- ts
    args$order <- order
    args$seasonal <- seasonal
    args$include.mean <- include.mean
    args$optim.method <- optim.method

    if (tolower(method) == "css") {
        do.call(.madlib.arima.css, args)
    } else
        stop("Right now MADlib's ARIMA does not support methods ",
             "other than \"CSS\" !")
})

## ----------------------------------------------------------------------

.madlib.arima.css <- function (formula, data, order=c(1,1,1),
                               seasonal = list(order = c(0, 0, 0),
                               period = NA),
                               include.mean = TRUE, optim.method = "LM",
                               tau = 1e-3, e1 = 1e-15, e2 = 1e-15,
                               e3 = 1e-15, max.iter = 100,
                               hessian.delta = 1e-4, param.init = "zero",
                               chunk.size = 10000, ...)
{
    if (tolower(optim.method) != "lm")
        stop("Right now MADlib ARIMA only supports ",
             "LM optimization method !")

    if (!identical(seasonal, list(order = c(0, 0, 0), period = NA)))
        stop("Right now MADlib ARIMA does not support seasonal order !")

    if (length(order) != 3)
        stop("ARIMA needs order to be an integer array of length 3 !")
    
    ## make sure fitting to db.obj
    if (! is(data, "db.obj"))
        stop("madlib.lm cannot be used on the object ",
             deparse(substitute(data)))
    
    ## Only newer versions of MADlib are supported
    .check.madlib.version(data, 1.2)

    conn.id <- conn.id(data)
    
    db.str <- (.get.dbms.str(conn.id))$db.str
    if (db.str == "HAWQ")
        stop("Right now MADlib on HAWQ does not support ARIMA !")
    
    warnings <- .suppress.warnings(conn.id)

    ## analyze the formula
    analyzer <- .get.params(formula, data)
    data <- analyzer$data
    params <- analyzer$params
    is.tbl.source.temp <- analyzer$is.tbl.source.temp
    tbl.source <- gsub("\"", "", content(data))
    
    if (length(params$ind.vars) != 1)
        stop("Only one time stamp is allowed !")

    ## dependent, independent and grouping strings
    if (is.null(params$grp.str))
        grp <- "NULL"
    else
        stop("Right now MADlib does not support grouping in ARIMA !")
    
    ## allow expressions as time series and time stamp
    ## create intermediate tables to accomodate this
    col.names <- names(data)
    if (!(.strip(params$ind.vars, "\"") %in% col.names) ||
        !(.strip(params$dep.str, "\"") %in% col.names)) {
        new.src <- .unique.string()
        res <- .get.res(sql = paste("create table ", new.src, " as ",
                        "select ", params$dep.str, " as tval, ",
                        params$ind.vars, " as tid from ", tbl.source,
                        sep = ""),
                        conn.id = conn.id)
        if (is.tbl.source.temp) delete(tbl.source)
        is.tbl.source.temp <- TRUE
        tbl.source <- new.src
        params$dep.str <- "tval"
        params$ind.vars <- "tid"
    }

    ## construct SQL string
    madlib <- schema.madlib(conn.id) # MADlib schema name
    tbl.output <- .unique.string()
    order.str <- paste("array[", toString(order), "]", sep = "")
    optim.control.str <- paste("tau=", tau, ", e1=", e1, ", e2=", e2,
                               ", e3=", e3, ", hessian_delta=",
                               hessian.delta, ", chunk_size=",
                               chunk.size, ", param_init=\"",
                               param.init, "\"", sep = "")
    sql <- paste("select ", madlib, ".arima_train('",
                 tbl.source, "', '", tbl.output, "', '",
                 params$ind.vars, "', '", params$dep.str, "', ",
                 grp, ", ", include.mean, ", ", order.str, ", '",
                 optim.control.str, "')", sep = "")

    ## execute and get the result
    res <- .get.res(sql=sql, conn.id=conn.id)

    p <- order[1]
    d <- order[2]
    q <- order[3]
    
    ## retrieve the coefficients
    res <- preview(tbl.output, conn.id=conn.id, "all")
    rst <- list()
    
    rst$coef <- numeric(0) # coefficients
    rst$s.e. <- numeric(0) # standard errors
    coef.names <- character(0)
    if (p != 0) {
        rst$coef <- c(rst$coef, res[1,1:p])
        rst$s.e. <- c(rst$s.e., res[1,p+(1:p)])
        coef.names <- c(coef.names, paste("ar", 1:p, sep = ""))
    }
    if (q != 0) {
        rst$coef <- c(rst$coef, res[1,2*p + (1:q)])
        rst$s.e. <- c(rst$s.e., res[1,2*p+q+(1:q)])
        coef.names <- c(coef.names, paste("ma", 1:q, sep = ""))
    }
    if (include.mean && d == 0) {
        rst$coef <- c(rst$coef, res[1,dim(res)[2]-1])
        rst$s.e. <- c(rst$s.e., res[1,dim(res)[2]])
        coef.names <- c(coef.names, "mean")
    }
    names(rst$coef) <- coef.names
    names(rst$s.e.) <- coef.names

    ## retrieve the statistics
    rst$series <- content(data)
    rst$time.stamp <- params$ind.vars
    rst$time.series <- params$dep.str

    res <- preview(paste(tbl.output, "_summary", sep = ""),
                   conn.id=conn.id, "all")
    rst$sigma2 <- res$residual_variance
    rst$loglik <- res$log_likelihood
    rst$iter.num <- res$iter_num
    rst$exec.time <- res$exec_time
    
    ## create db.data.frame object for residual table
    rst$residuals <- db.data.frame(paste(tbl.output, "_residual", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    rst$model <- db.data.frame(tbl.output, conn.id = conn.id,
                               verbose = FALSE)
    rst$statistics <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                    conn.id = conn.id, verbose = FALSE)

    ## If temp.source is TRUE, the delete(...) function
    ## will delete it
    if (is.tbl.source.temp) rst$temp.source <- TRUE
    else rst$temp.source <- FALSE
                
    .restore.warnings(warnings)

    class(rst) <- "arima.css.madlib"
    rst
}

## ----------------------------------------------------------------------

summary.arima.css.madlib <- function (object, ...)
{
    object
}

## ----------------------------------------------------------------------

print.arima.css.madlib <- function (x,
                                    digits = max(3L,
                                    getOption("digits") - 3L),
                                    ...)
{
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
    cat("Coefficients:\n")

    coef <- format(x$coef, digits = digits)
    std.err <- format(x$s.e., digits = digits)
    est <- rbind(coef, std.err)
    row.names(est) <- c("", "s.e.")

    print(format(data.frame(est), justify = "right"))

    cat("\n")

    cat("sigma^2 estimated as ", format(x$sigma2, digits = digits),
        ",  part log likelihood = ", format(x$loglik, digits = digits),
        sep = "")
    cat("\n")
}

## -----------------------------------------------------------------------

show.arima.css.madlib <- function (object)
{
    print(object)
}

## ----------------------------------------------------------------------

## Some functionalities will be implemented in the future
predict.arima.css.madlib <- function(object, n.ahead = 1, ...)
{
    conn.id <- conn.id(object$model)
    
    warnings <- .suppress.warnings(conn.id)
    
    tbl.output <- .unique.string()
    tbl.model <- .strip(content(object$model), "\"")
    madlib <- schema.madlib(conn.id) # MADlib schema name
    sql <- paste("select ", madlib, ".arima_forecast('",
                 tbl.model, "', '", tbl.output, "',",
                 n.ahead, ")", sep = "")
    res <- .get.res(sql=sql, conn.id=conn.id)
    rst <- db.data.frame(tbl.output, conn.id=conn.id, verbose = FALSE)

    .restore.warnings(warnings)
    
    rst
}
