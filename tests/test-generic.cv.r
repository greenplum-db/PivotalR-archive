context("Test generic.cv")

.get.param.inputs(c("port", "dbname"))
cid <- db.connect(port = port, dbname = dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## ----------------------------------------------------------------------

test_that("Install-check should run without error", {
    expect_that(
    {
        x <- matrix(rnorm(100*20),100,20)
        y <- rnorm(100, 0.1, 2)

        dat <- data.frame(x, y)
        delete("eldata")
        z <- as.db.data.frame(dat, "eldata", verbose = FALSE)

        g <- generic.cv(
            train = function (data, alpha, lambda) {
                madlib.elnet(y ~ ., data = data, family = "gaussian", alpha =
                             alpha, lambda = lambda, control = list(random.stepsize=TRUE))
            },
            predict = predict,
            metric = function (predicted, data) {
                lk(mean((data$y - predicted)^2))
            },
            data = z,
            params = list(alpha=1, lambda=seq(0,0.2,0.1)),
            k = 5, find.min = TRUE, verbose = FALSE)
    }, has_no_error())

    expect_that(
    {
        err <- generic.cv(
            function(data) {
                madlib.lm(rings ~ . - id - sex, data = data)
            },
            predict,
            function(predicted, data) {
                lookat(mean((data$rings - predicted)^2))
            }, data = dat, verbose = FALSE)
    }, has_no_error())
})

## ----------------------------------------------------------------------

db.disconnect(cid, verbose = FALSE)
