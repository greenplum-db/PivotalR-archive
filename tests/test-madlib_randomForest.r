context("Test cases for madlib.randomForest and its helper functions")

## ------------------------------------------------------------
## Test preparations
library(randomForest)
library(MASS)

.get.param.inputs(c(".port", ".dbname"))
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
dat.r <- abalone

## The tests
test_that("Test randomForest", {
          fit.db <- madlib.randomForest(sex ~ . - id, data = dat, importance=TRUE, id = dat.id)
          fit.r <- summary(randomForest(sex ~ . - id, data = dat.r, mtry = 2, importance = TRUE))

          expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
})