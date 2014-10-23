context("Test cases for madlib.glm and its helper functions")

## ------------------------------------------------------------
## Test preparations

.get.param.inputs(c(".port", ".dbname"))
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
dat.r <- abalone

## ------------------------------------------------------------
## The actual tests

test_that("Test gaussian(identity)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = gaussian(identity), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = gaussian(identity)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(identity) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = gaussian(identity), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = gaussian(identity)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(identity) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = gaussian(identity), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = gaussian(identity))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test gaussian(log)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = gaussian(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = gaussian(log)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(log) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = gaussian(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = gaussian(log)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(log) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = gaussian(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = gaussian(log))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test gaussian(inverse)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = gaussian(inverse), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = gaussian(inverse)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(inverse) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = gaussian(inverse), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = gaussian(inverse)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test gaussian(inverse) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = gaussian(inverse), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = gaussian(inverse))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test binomial(logit)", {
          fit.db <- madlib.glm(rings < 10 ~ . - id - sex, data = dat, family = binomial(logit), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings < 10 ~ . - id - sex, data = dat.r, family = binomial(logit)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test binomial(logit) with categorical features", {
          fit.db <- madlib.glm(rings < 10 ~ . - id, data = dat, family = binomial(logit), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings < 10 ~ . - id, data = dat.r, family = binomial(logit)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test binomial(logit) with grouping", {
          fit.db <- madlib.glm(rings < 10 ~ . - id | sex, data = dat, family = binomial(logit), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings < 10 ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = binomial(logit))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test binomial(probit)", {
          fit.db <- madlib.glm(rings < 10 ~ . - id - sex, data = dat, family = binomial(probit), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings < 10 ~ . - id - sex, data = dat.r, family = binomial(probit)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test binomial(probit) with categorical features", {
          fit.db <- madlib.glm(rings < 10 ~ . - id, data = dat, family = binomial(probit), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings < 10 ~ . - id, data = dat.r, family = binomial(probit)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test binomial(probit) with grouping", {
          fit.db <- madlib.glm(rings < 10 ~ . - id | sex, data = dat, family = binomial(probit), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings < 10 ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = binomial(probit))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test poisson(log)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = poisson(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = poisson(log)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test poisson(log) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = poisson(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = poisson(log)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test poisson(log) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = poisson(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = poisson(log))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

# test_that("Test poisson(identity)", {
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = poisson(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = poisson(identity)))
#
#           expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
#           expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
# })

# test_that("Test poisson(identity) with categorical features", {
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = poisson(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = poisson(identity)))
#
#           expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
#           expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
# })

test_that("Test poisson(identity) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = poisson(identity), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = poisson(identity))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

test_that("Test poisson(sqrt)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = poisson(sqrt), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = poisson(sqrt)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test poisson(sqrt) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = poisson(sqrt), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = poisson(sqrt)))

          expect_that(fit.db$coef,    is_equivalent_to(fit.r$coefficients[ , 1]))
          expect_that(fit.db$std_err, is_equivalent_to(fit.r$coefficients[ , 2]))
})

test_that("Test poisson(sqrt) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = poisson(sqrt), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = poisson(sqrt))))

          expect_that(fit.db[[1]]$coef,    is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[1]]$std_err, is_equivalent_to(fit.r[[1]]$coefficients[ , 2]))
          expect_that(fit.db[[2]]$coef,    is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$std_err, is_equivalent_to(fit.r[[2]]$coefficients[ , 2]))
          expect_that(fit.db[[3]]$coef,    is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$std_err, is_equivalent_to(fit.r[[3]]$coefficients[ , 2]))
})

## ------------------------------------------------------------

# test_that("Test Gamma(inverse)", {
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(inverse)))
#
#           expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
# })
#
# test_that("Test Gamma(inverse) with categorical features", {
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(inverse)))
#
#           expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
# })

test_that("Test Gamma(inverse) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = Gamma(inverse))))

          expect_that(fit.db[[1]]$coef, is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$coef, is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$coef, is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
})

## ------------------------------------------------------------

# test_that("Test Gamma(identity)", {
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(identity)))
#
#           expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
# })
#
# test_that("Test Gamma(identity) with categorical features", {
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(identity)))
#
#           expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
# })

test_that("Test Gamma(identity) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = Gamma(identity))))

          expect_that(fit.db[[1]]$coef, is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$coef, is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$coef, is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
})

## ------------------------------------------------------------

test_that("Test Gamma(log)", {
          fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = Gamma(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(log)))

          expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
})

test_that("Test Gamma(log) with categorical features", {
          fit.db <- madlib.glm(rings ~ . - id, data = dat, family = Gamma(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(log)))

          expect_that(fit.db$coef, is_equivalent_to(fit.r$coefficients[ , 1]))
})

test_that("Test Gamma(log) with grouping", {
          fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = Gamma(log), control = list(max.iter = 20, use.glm = T))
          fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = Gamma(log))))

          expect_that(fit.db[[1]]$coef, is_equivalent_to(fit.r[[1]]$coefficients[ , 1]))
          expect_that(fit.db[[2]]$coef, is_equivalent_to(fit.r[[2]]$coefficients[ , 1]))
          expect_that(fit.db[[3]]$coef, is_equivalent_to(fit.r[[3]]$coefficients[ , 1]))
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
