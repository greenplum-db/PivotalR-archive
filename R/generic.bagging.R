
## ------------------------------------------------------------------------
## Bagging method, not a wrapper of MADlib function
## ------------------------------------------------------------------------

generic.bagging <- function (train, data, nbags = 10, fraction = 1)
{
    if (fraction > 1)
        stop("fraction cannot be larger than 1!")
    if (!is(data, "db.obj"))
        stop("data must be a db.obj!")
    
    n <- dim(data)[1]
    size <- as.integer(n * fraction)

    for (i in 1:nbags) {
        data.use <- sample(data, size, replace = TRUE)
        if (i == 1) 
            res <- train(data = data.use)
        else
            res <- c(res, train(data = data.use))
    }
    class(res) <- "bagging.model"
}

## ------------------------------------------------------------------------

predict.bagging.model <- function (model, newdata, combine = "average")
{
    l <- length(model)
    for (i in seq_Len(l)) {
        if (i == 1)
            pred <- predict(model[i], newdata)
        else
            pred <- c(pred, predict(model[i], newdata))
    }

    if (combine == "average") {
        for (i in seq_len(l)) {
            if (i == 1)
                res <- pred[i]
            else
                res <- res + pred[i]
        }
        res / l
    } else if (combine == "vote") {
        stop("To be implemented!")
    } else
        stop("combine method must be \"average\" or \"vote\"!")
}

