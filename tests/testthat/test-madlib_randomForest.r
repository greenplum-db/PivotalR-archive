context("Test cases for madlib.randomForest and its helper functions")

## ------------------------------------------------------------
## Test preparations

env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)

cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
dat.r <- abalone

## The tests
test_that("Test randomForest", {
    testthat::skip_on_cran()
    expect_error(fit.db <- madlib.randomForest(sex ~ . - id,
                                               data = dat, id = 'id'), NA)
})