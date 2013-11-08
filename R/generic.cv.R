
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
            fits <- train(data = cuts$train[[i]])
            pred <- predict(object = fits, newdata = cuts$valid[[i]])
            err <- c(err, as.numeric(metric(predicted = pred, actual = cuts$valid[[i]])))
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
        return (data.frame(err = mean(err), err.std = sd(err)))
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
                arg.list$data <- cuts$train[[i]]
                fits <- do.call(train, arg.list)
                pred <- predict(object = fits, newdata = cuts$valid[[i]])
                err.k <- c(err.k, as.numeric(metric(predicted = pred,
                                                    actual = cuts$valid[[i]])))
                delete(fits)
            }
            err <- rbind(err, err.k)
        }

        rownames(args) <- NULL
        args <- as.data.frame(args)
        names(args) <- arg.names

        if (verbose) cat("Done.\n")
        rst <- list(avg = colMeans(err), std = .colSds(err), vals = err)
        if (verbose) cat("Fitting the best model using the whole data set ... ")
        if (find.min) best <- which.min(rst$avg)
        else best <- which.max(rst$avg)
        arg.list <- .create.args(arg.names, params, best)
        arg.list$data <- data
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
        list(errs = rst, params = args, best = best.fit, best.params = arg.list)
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
        if (identical(dist.cols, character(0))) {
            dist.str <- paste("distributed by (", id.col, ")", sep = "")
            dist.by <- id.col
        } else {
            dist.str <- paste("distributed by (", dist.cols, ")", sep = "")
            dist.by <- dist.cols
        }
    } else {
        dist.str <- ""
        dist.by <- ""
    }
    .db.getQuery(
        .format("create temp table <tmp> as
                     select *,
                         random() as <id.col>
                     from (<tbl>) s <dist.str>",
                list(tmp=tmp, id.col=id.col,
                     tbl=content(x[,]), dist.str=dist.str)),
        conn.id = conn.id)
    y <- db.data.frame(tmp, conn.id = conn.id, is.temp = TRUE,
                       verbose = FALSE)
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
