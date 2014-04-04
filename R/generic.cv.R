## -----------------------------------------------------------------------
## Generic cross-validation, not a wrapper of MADlib function
## -----------------------------------------------------------------------

generic.cv <- function (train, predict, metric, data,
                        params = NULL, k = 10, approx.cut = TRUE,
                        verbose = TRUE, find.min = TRUE)
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
        if (approx.cut)
            cuts <- .approx.cut.data(data, k)
        else
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
            fits <- train(cuts$train[[i]])
            pred <- predict(fits, cuts$valid[[i]])
            err <- c(err, as.numeric(metric(pred, cuts$valid[[i]])))
            if (is(data, "db.obj")) delete(fits)
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
        return (list(err = mean(err), err.std = sd(err)))
    } else {
        arg.names <- names(params)
        l <- max(sapply(params, length))
        err <- numeric(0)
        for (i in 1:k) {
            if (verbose) cat("Running on fold", i, "now ...\n")
            err.k <- numeric(0)
            for (j in 1:l) {
                arg.list <- .create.args(arg.names, params, j)
                if (verbose) cat("    parameters", toString(arg.list), "...\n")
                if (i == 1) {
                    if (j == 1)
                        args <- as.vector(unlist(arg.list))
                    else
                        args <- rbind(args, as.vector(unlist(arg.list)))
                }
                arg.list[[length(arg.list)+1]] <- cuts$train[[i]]
                fits <- do.call(train, arg.list)
                pred <- predict(fits, cuts$valid[[i]])
                err.k <- c(err.k, as.numeric(metric(pred, cuts$valid[[i]])))
                if (is(data, "db.obj")) delete(fits)
            }
            err <- rbind(err, err.k)
        }

        rownames(args) <- NULL
        args <- as.data.frame(args)
        names(args) <- arg.names

        if (verbose) cat("Done.\n")
        rownames(err) <- NULL
        rst <- list(avg = colMeans(err), std = .colSds(err), vals = err)
        if (verbose) cat("Fitting the best model using the whole data set ... ")
        if (find.min) best <- which.min(rst$avg)
        else best <- which.max(rst$avg)

        arg.list <- .create.args(arg.names, params, best)
        arg.list[[length(arg.list) + 1]] <- data
        best.fit <- do.call(train, arg.list)
        arg.list$data <- NULL
        if (verbose) cat("Done.\n")
        if (is(data, "db.obj")) {
            if (verbose) cat("Cleaning up ...\n")
            for (i in 1:k) {
                delete(cuts$train[[i]])
                delete(cuts$valid[[i]])
            }
            delete(cuts$inter)
            .restore.warnings(warnings)
        }
        res <- list(metric = rst, params = args, best = best.fit, best.params = arg.list)
        class(res) <- "cv.generic"
        res
    }
}

## -----------------------------------------------------------------------

.create.args <- function (arg.names, params, i)
{
    res <- list()
    for (k in seq_len(length(arg.names))) {
        l <- length(params[[arg.names[k]]])
        j <- (i-1) %% l + 1
        res[[arg.names[k]]] <- params[[arg.names[k]]][j]
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

## ----------------------------------------------------------------------

## cut the data in an approximate way, but faster
.approx.cut.data <- function (x, k)
{
    conn.id <- conn.id(x)
    tmp <- .unique.string()
    id.col <- .unique.string()
    dbms <- (.get.dbms.str(conn.id))$db.str
    if (dbms != "PostgreSQL") {
        dist.cols <- x@.dist.by
        if (identical(dist.cols, character(0)) ||
            !all(dist.cols %in% x@.col.name)) {
            dist.str <- paste("distributed by (", id.col, ")", sep = "")
            dist.by <- id.col
        } else {
            dist.str <- paste("distributed by (",
                              paste("\"", dist.cols, "\"", sep = ""),
                              ")", sep = "")
            dist.by <- dist.cols
        }
    } else {
        dist.str <- ""
        dist.by <- ""
    }

    random.col <- x[,1]
    random.col@.expr <- "random()"
    random.col@.col.name <- id.col
    random.col@.col.data_type <- "double precision"
    random.col@.col.udt_name <- "float8"
    random.col@.is.factor <- FALSE
    random.col@.factor.ref <- as.character(NA)
    random.col@.factor.suffix <- ""
    random.col@.content <- gsub("select\\s+.*\\s+from",
                                paste("select random() as", id.col, "from"),
                                random.col@.content)

    x[[id.col]] <- random.col
    y <- as.db.data.frame(x, is.temp = TRUE, verbose = FALSE, pivot = FALSE)
    id <- ncol(y)
    size <- 1 / k
    tick <- c(0, seq(size, length.out = k-1, by = size), 1)
    valid <- list()
    train <- list()
    for (i in 1:k) {
        valid[[i]] <- y[y[,id]>tick[i] & y[,id]<=tick[i+1],-id]
        train[[i]] <- y[!(y[,id]>tick[i] & y[,id]<=tick[i+1]),-id]
    }
    list(train = train, valid = valid, inter = y, dist.by = dist.by)
}

## ## ----------------------------------------------------------------------

## plot.cv.generic <- function (x, ...)
## {
##     cvobj = x
##     xlab = "params"
##     plot.args = list(x = , y = x$metric$avg,
##     ylim = range(cvobj$cvup, cvobj$cvlo), xlab = xlab, ylab = cvobj$name,
##     type = "n")
##     new.args = list(...)
##     if (length(new.args))
##         plot.args[names(new.args)] = new.args
##     do.call("plot", plot.args)
##     error.bars(sign.lambda * log(cvobj$lambda), cvobj$cvup, cvobj$cvlo,
##         width = 0.01, col = "darkgrey")
##     points(sign.lambda * log(cvobj$lambda), cvobj$cvm, pch = 20,
##         col = "red")
##     axis(side = 3, at = sign.lambda * log(cvobj$lambda), labels = paste(cvobj$nz),
##         tick = FALSE, line = 0)
##     abline(v = sign.lambda * log(cvobj$lambda.min), lty = 3)
##     abline(v = sign.lambda * log(cvobj$lambda.1se), lty = 3)
##     invisible()
## }
