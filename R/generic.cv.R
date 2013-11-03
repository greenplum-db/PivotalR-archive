
## -----------------------------------------------------------------------
## Generic cross-validation, not a wrapper of MADlib function
## -----------------------------------------------------------------------

generic.cv <- function (train, predict, metric, data,
                        params = NULL, k = 10, approx.cut = TRUE)
{
    if (!is(data, "db.obj"))
        stop("data must be a db.obj!")
    if (!is.null(params) && (!is.list(params) || is.data.frame(params)))
        stop("params must be a list!")

    conn.id <- conn.id(data)
    warnings <- .suppress.warnings(conn.id)

    if (approx.cut)
        cuts <- .approx.cut.data(data, k)
    else
        cuts <- .cut.data(data, k)
    for (i in 1:k) {
        cuts$train[[i]] <- as.db.data.frame(cuts$train[[i]], .unique.string(),
                                            FALSE, FALSE, TRUE, FALSE,
                                            cuts$dist.by, NULL)
        cuts$valid[[i]] <- as.db.data.frame(cuts$valid[[i]], .unique.string(),
                                            FALSE, FALSE, TRUE, FALSE,
                                            cuts$dist.by, NULL)
    }
    conn.id <- conn.id(cuts$train[[1]])

    if (is.null(params)) {
        err <- numeric(0)
        for (i in 1:k) {
            fits <- train(data = cuts$train[[i]])
            pred <- predict(fits, newdata = cuts$valid[[i]])
            err <- c(err, as.numeric(metric(predicted = pred, data = cuts$valid[[i]])))
            delete(fits)
        }
        
        for (i in 1:k) {
            delete(content(cuts$train[[i]]), conn.id, TRUE)
            delete(content(cuts$valid[[i]]), conn.id, TRUE)
        }
        delete(content(cuts$inter), conn.id, TRUE)

        .restore.warnings(warnings)
        
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

        for (i in 1:k) {
            delete(cuts$train[[i]])
            delete(cuts$valid[[i]])
        }
        delete(cuts$inter)

        .restore.warnings(warnings)
        
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

## ----------------------------------------------------------------------

## cut the data in an approximate way, but faster
.approx.cut.data <- function (x, k)
{
    size <- 100
    n <- k * size
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
                         trunc(random()*<n>+1) as <id.col>
                     from (<tbl>) s <dist.str>",
                list(tmp=tmp, n=n, id.col=id.col,
                     tbl=content(x[,]), dist.str=dist.str)),
        conn.id = conn.id)
    y <- db.data.frame(tmp, conn.id = conn.id, is.temp = TRUE)
    id <- ncol(y)
    tick <- c(0, seq(size, length.out = k-1, by = size), n)
    valid <- list()
    train <- list()
    for (i in 1:k) {
        valid[[i]] <- y[y[,id]>tick[i] & y[,id]<=tick[i+1],-id]
        train[[i]] <- y[!(y[,id]>tick[i] & y[,id]<=tick[i+1]),-id]
    }
    list(train = train, valid = valid, inter = y, dist.by = dist.by)
}
