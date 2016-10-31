context("Test cases for madlib.lm and its helper functions")

## ------------------------------------------------------------
## Test preparations

env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)

cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
dat.r <- abalone


test_that("madlib.lm gives correct results", {
          fit.db <- madlib.lm(rings ~ . - id - sex, data = dat, control = list(max.iter = 20, use.lm = T))
          fit.r <- summary(lm(rings ~ . - id - sex, data = dat.r))

          expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
})

test_that("madlib.lm gives correct results with categorical features", {
          fit.db <- madlib.lm(rings ~ . - id, data = dat, control = list(max.iter = 20, use.lm = T))
          fit.r <- summary(lm(rings ~ . - id, data = dat.r))

          expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
})

test_that("madlib.lm gives correct results with grouping", {
          fit.db <- madlib.lm(rings ~ . - id | sex, data = dat, control = list(max.iter = 20, use.lm = T))
          fit.r <- lapply(1:3, function(i) summary(lm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2])))

          expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
})