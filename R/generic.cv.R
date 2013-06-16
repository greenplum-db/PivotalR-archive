
## ------------------------------------------------------------------------
## Generic cross-validation, not a wrapper of MADlib function
## ------------------------------------------------------------------------

generic.cv <- function (train, predict, metric, data,
                        params = NULL, k = 10)
{
    if (!is(data, "db.obj"))
        stop("data must be a db.obj!")
    if (!is.null(params) && (!is.list(params) || is.data.frame(params)))
        stop("params must be a list!")
    
    cuts <- .cut.data(data, k)

    if (is.null(params)) {
        err <- numeric(0)
        for (i in 1:k) {
            fits <- train(data = cuts$train[i])
            pred <- predict(fits, data = cuts$valid[i])
            err <- c(err, metric(predicted = pred, data = cuts$valid[i]))
        }
        data.frame(err = mean(err), err.std = sd(err))
    } else {
        arg.names <- names(params)
        l <- 0
        for (i in seq_len(length(params)))
            if (length(params[[i]]) > l) l <- length(params[[i]])
        err <- numeric(0)
        for (i in 1:k) {
            err.k <- numeric(0)
            for (j in 1:l) {
                arg.list <- .create.args(arg.names, j)
                arg.list$data <- cuts$train[i]
                fits <- do.call(train, arg.list)
                pred <- predict(fits, data = cuts$valid[i])
                err.k <- c(err.k, metric(predicted = pred,
                                         data = cuts$valid[i]))
            }
            err <- rbind(err, err.k)
        }
        data.frame(err = colMeans(err), err.std = .colSds(err))
    }
}

## ------------------------------------------------------------------------

.create.args <- function (arg.names, params, i)
{
    res <- list()
    for (i in seq_len(length(arg.names))) {
        l <- length(params[[arg.names[i]]])
        j <- i %% l
        res[[arg.names[i]]] <- params[[arg.names[i]]][j]
    }
    res
}

## ------------------------------------------------------------------------

.colSds <- function (dat)
{
    std <- rep(0, dim(dat)[2])
    for (i in 1:(dim(dat)[2]))
        std[i] <- sd(dat[,i])
    std
}
