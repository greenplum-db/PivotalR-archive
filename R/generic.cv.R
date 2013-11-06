
## -----------------------------------------------------------------------
## Generic cross-validation, not a wrapper of MADlib function
## -----------------------------------------------------------------------

generic.cv <- function (train, predict, metric, data,
                        params = NULL, k = 10, verbose = TRUE)
{
    if (!is.null(params) && (!is.list(params) || is.data.frame(params)))
        stop("params must be a list!")

    if (is(data, "db.obj")) {
        conn.id <- conn.id(data)
        warnings <- .suppress.warnings(conn.id)
        if (verbose) {
            message("Computation in-database ...")
            cat("Cutting the data row-wise into", k, "pieces ...\n")
        }
        cuts <- .cut.data(data, k)
        for (i in 1:k) {
            cuts$train[[i]] <- as.db.data.frame(cuts$train[[i]], .unique.string(),
                                                FALSE, FALSE, TRUE, FALSE, NULL,
                                                NULL)
            cuts$valid[[i]] <- as.db.data.frame(cuts$valid[[i]], .unique.string(),
                                                FALSE, FALSE, TRUE, FALSE, NULL,
                                                NULL)
        }
    } else {
        if (verbose) {
            message("Computation in memory ...")
            cat("Cutting the data row-wise into", k, "pieces ...\n")
        }
        n <- nrow(data)
        idx <- sample(seq_len(n), n)
        sz <- n %/% k
        cuts <- list(train = list(), valid = list())
        for (i in seq_len(k)) {
            if (i == k) {
                range.v <- ((k-1) * sz):n
            } else {
                range.v <- (1:sz) + (i-1)*sz
            }
            range.t <- setdiff(1:n, range.v)
            cuts$train[[i]] <- data[idx[range.t],]
            cuts$valid[[i]] <- data[idx[range.v],]
        }        
    }

    if (is.null(params)) {
        err <- numeric(0)
        for (i in 1:k) {
            if (verbose) cat("Running on fold", i, "now ...\n")
            fits <- train(data = cuts$train[[i]])
            pred <- predict(fits, newdata = cuts$valid[[i]])
            err <- c(err, as.numeric(metric(predicted = pred, data = cuts$valid[[i]])))
            delete(fits)
        }

        if (is(data, "db.obj")) {
            if (verbose) cat("Cleaning up ...\n")
            for (i in 1:k) {
                delete(content(cuts$train[[i]]), conn.id, TRUE)
                delete(content(cuts$valid[[i]]), conn.id, TRUE)
            }
            delete(content(cuts$inter), conn.id, TRUE)
            .restore.warnings(warnings)
        } 
        
        if (verbose) cat("Done.\n")
        data.frame(err = mean(err), err.std = sd(err))
    } else {
        arg.names <- names(params)
        l <- 0
        for (i in seq_len(length(params)))
            if (length(params[[i]]) > l) l <- length(params[[i]])
        err <- numeric(0)
        for (i in 1:k) {
            if (verbose) cat("Running on fold", i, "now ...\n")
            err.k <- numeric(0)
            for (j in 1:l) {
                arg.list <- .create.args(arg.names, j)
                if (verbose) cat("    parameters", toString(arg.list), "...\n")
                if (i == 1) {
                    if (j == 1)
                        args <- as.vector(unlist(arg.list))
                    else
                        args <- rbind(args, as.vector(unlist(arg.list)))
                }
                arg.list$data <- cuts$train[[i]]
                fits <- do.call(train, arg.list)
                pred <- predict(fits, newdata = cuts$valid[[i]])
                err.k <- c(err.k, as.numeric(metric(predicted = pred,
                                                    data = cuts$valid[[i]])))
                delete(fits)
            }
            err <- rbind(err, err.k)
        }

        args <- as.data.frame(args)
        names(args) <- arg.names

        if (is(data, "db.obj")) {
            if (verbose) cat("Cleaning up ...\n")
            for (i in 1:k) {
                delete(cuts$train[[i]])
                delete(cuts$valid[[i]])
            }
            delete(cuts$inter)
            .restore.warnings(warnings)
        }

        if (verbose) cat("Done.\n")
        cbind(args,
              data.frame(err = colMeans(err), err.std = .colSds(err)))
    }
}

## -----------------------------------------------------------------------

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

## -----------------------------------------------------------------------

.colSds <- function (dat)
{
    std <- rep(0, dim(dat)[2])
    for (i in 1:(dim(dat)[2]))
        std[i] <- sd(dat[,i])
    std
}
