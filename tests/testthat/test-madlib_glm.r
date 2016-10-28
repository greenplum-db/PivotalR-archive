context("Test cases for madlib.glm and its helper functions")

## ------------------------------------------------------------
## Test preparations

# get.param.inputs(c(".port", ".dbname"))
env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)

cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(abalone, table.name = .unique.string(), conn.id = cid, verbose = FALSE, is.temp = FALSE)
dat.r <- abalone


## ------------------------------------------------------------
## The actual tests

test_that("Test gaussian(identity)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                       family = gaussian(identity),
                       control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r,
                       family = gaussian(identity)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
      }
)

test_that("Test gaussian(identity) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id, data = dat,
                         family = gaussian(identity),
                         control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id, data = dat.r,
                         family = gaussian(identity)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    }
)

test_that("Test gaussian(identity) with grouping", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                         family = gaussian(identity),
                         control = list(max.iter = 20, use.glm = T))
    fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id,
                                                 data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                                                 family = gaussian(identity))))

    expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    })

## ------------------------------------------------------------

test_that("Test gaussian(log)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                         family = gaussian(log),
                         control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = gaussian(log)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    }
)

test_that("Test gaussian(log) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id, data = dat, family = gaussian(log), control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = gaussian(log)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
    }
)

test_that("Test gaussian(log) with grouping", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = gaussian(log), control = list(max.iter = 20, use.glm = T))
    fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = gaussian(log))))

    expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
    }
)

## ------------------------------------------------------------

test_that("Test gaussian(inverse)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                         family = gaussian(inverse),
                         control = list(max.iter = 100, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = gaussian(inverse)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-1, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    })

test_that("Test gaussian(inverse) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id, data = dat,
                         family = gaussian(inverse),
                         control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id, data = dat.r,
                         family = gaussian(inverse)))

          # expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
          #              tolerance=1e-2, check.attributes=FALSE)
          expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                       tolerance=1e-2, check.attributes=FALSE)
          })

test_that("Test gaussian(inverse) with grouping", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                         family = gaussian(inverse),
                         control = list(max.iter = 20, use.glm = T))
    fit.r <- lapply(1:3, function(i)
                    summary(glm(rings ~ . - id,
                                data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                                family = gaussian(inverse))))

    expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    })

    ## ------------------------------------------------------------

test_that("Test binomial(logit)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id - sex, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = T))
    fit.r <- summary(glm(rings < 10 ~ . - id - sex,
                         data = dat.r, family = binomial(logit)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$nobs, nrow(dat.r))
    })

test_that("Test binomial(logit) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = T))
    fit.r <- summary(glm(rings < 10 ~ . - id, data = dat.r,
                         family = binomial(logit)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test binomial(logit) with grouping", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id | sex, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = T))
    model.r <- lapply(1:3, function(i) glm(rings < 10 ~ . - id,
                                           data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                                           family = binomial(logit)))
    fit.r <- lapply(1:3, function(i) summary(model.r[[i]]))

    expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$nobs, nrow(model.r[[1]]$data))
    expect_equal(fit.db[[2]]$nobs, nrow(model.r[[2]]$data))
    expect_equal(fit.db[[3]]$nobs, nrow(model.r[[3]]$data))

  })


##Test same as above but with use.glm=F

test_that("Test binomial(logit)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id - sex, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = F))
    fit.r <- summary(glm(rings < 10 ~ . - id - sex, data = dat.r,
                         family = binomial(logit)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$nobs, nrow(dat.r))
    })

test_that("Test binomial(logit) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = F))
    fit.r <- summary(glm(rings < 10 ~ . - id, data = dat.r,
                         family = binomial(logit)))
    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test binomial(logit) with grouping", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 10 ~ . - id | sex, data = dat,
                         family = binomial(logit),
                         control = list(max.iter = 40, use.glm = F))
    model.r <- lapply(1:3, function(i)
                      glm(rings < 10 ~ . - id,
                          data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                          family = binomial(logit)))
    fit.r <- lapply(1:3, function(i) summary(model.r[[i]]))

    expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db[[1]]$nobs, nrow(model.r[[1]]$data))
    expect_equal(fit.db[[2]]$nobs, nrow(model.r[[2]]$data))
    expect_equal(fit.db[[3]]$nobs, nrow(model.r[[3]]$data))

    }
)

## ------------------------------------------------------------

test_that("Test binomial(probit)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 14 ~ . - id - sex, data = dat,
                         family = binomial(probit),
                         control = list(max.iter = 40, use.glm = T))
    fit.r <- summary(glm(rings < 14 ~ . - id - sex, data = dat.r,
                         family = binomial(probit)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
  }
)

test_that("Test binomial(probit) with categorical features", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings < 14 ~ . - id, data = dat,
                         family = binomial(probit),
                         control = list(max.iter = 40, use.glm = T))
    fit.r <- summary(glm(rings < 14 ~ . - id, data = dat.r,
                         family = binomial(probit)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
    }
)

## ------------------------------------------------------------

test_that("Test poisson(log)", {
    testthat::skip_on_cran()
    fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                         family = poisson(log), control = list(max.iter = 20, use.glm = T))
    fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r,
                         family = poisson(log)))

    expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
                 tolerance=1e-2, check.attributes=FALSE)
    expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
                 tolerance=1e-2, check.attributes=FALSE)
})

test_that("Test poisson(log) with categorical features", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id, data = dat,
                       family = poisson(log),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- summary(glm(rings ~ . - id, data = dat.r,
                       family = poisson(log)))

  expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test poisson(log) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                       family = poisson(log),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i)
                  summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                              family = poisson(log))))

  expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  })

## ------------------------------------------------------------

# test_that("Test poisson(identity)", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = poisson(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = poisson(identity)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
#           expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
# })

# test_that("Test poisson(identity) with categorical features", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = poisson(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = poisson(identity)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
#           expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
# })

test_that("Test poisson(identity) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                       family = poisson(identity),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i)
                  summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                              family = poisson(identity))))

  expect_equal(fit.db[[1]]$coef, expected=fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-1, check.attributes=FALSE,
               scale=mean(fit.r[[1]]$coefficients[ , 1]))
  expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE,
               scale=mean(fit.r[[1]]$coefficients[ , 2]))
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE,
               scale=mean(fit.r[[2]]$coefficients[ , 1]))
  expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE,
               scale=mean(fit.r[[2]]$coefficients[ , 2]))
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-1, check.attributes=FALSE,
               scale=mean(fit.r[[3]]$coefficients[ , 1]))
  expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE,
               scale=mean(fit.r[[3]]$coefficients[ , 2]))
  })

## ------------------------------------------------------------

test_that("Test poisson(sqrt)", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                       family = poisson(sqrt),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r,
                       family = poisson(sqrt)))

  expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test poisson(sqrt) with categorical features", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id, data = dat,
                       family = poisson(sqrt),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = poisson(sqrt)))

  expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db$std_err, fit.r$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test poisson(sqrt) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                       family = poisson(sqrt),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i)
                  summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                              family = poisson(sqrt))))

  expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[1]]$std_err, fit.r[[1]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$std_err, fit.r[[2]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$std_err, fit.r[[3]]$coefficients[ , 2],
               tolerance=1e-2, check.attributes=FALSE)
  })

## ------------------------------------------------------------

# test_that("Test Gamma(inverse)", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(inverse)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
# })
#
# test_that("Test Gamma(inverse) with categorical features", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(inverse)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
# })

test_that("Test Gamma(inverse) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = Gamma(inverse), control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i)
                  summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2],
                              family = Gamma(inverse))))

  expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  })

## ------------------------------------------------------------

# test_that("Test Gamma(identity)", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id - sex, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(identity)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
# })
#
# test_that("Test Gamma(identity) with categorical features", {
    testthat::skip_on_cran()
#           fit.db <- madlib.glm(rings ~ . - id, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
#           fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(identity)))
#
#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
# })

test_that("Test Gamma(identity) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat, family = Gamma(identity), control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id,
                                               data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = Gamma(identity))))

  expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  })

## ------------------------------------------------------------

test_that("Test Gamma(log)", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id - sex, data = dat,
                       family = Gamma(log),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- summary(glm(rings ~ . - id - sex, data = dat.r, family = Gamma(log)))

  expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test Gamma(log) with categorical features", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id, data = dat,
                       family = Gamma(log),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- summary(glm(rings ~ . - id, data = dat.r, family = Gamma(log)))

  expect_equal(fit.db$coef, fit.r$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  })

test_that("Test Gamma(log) with grouping", {
    testthat::skip_on_cran()
  fit.db <- madlib.glm(rings ~ . - id | sex, data = dat,
                       family = Gamma(log),
                       control = list(max.iter = 20, use.glm = T))
  fit.r <- lapply(1:3, function(i) summary(glm(rings ~ . - id, data = dat.r[dat.r$sex == fit.db[[i]]$sex, -2], family = Gamma(log))))

  expect_equal(fit.db[[1]]$coef, fit.r[[1]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[2]]$coef, fit.r[[2]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  expect_equal(fit.db[[3]]$coef, fit.r[[3]]$coefficients[ , 1],
               tolerance=1e-2, check.attributes=FALSE)
  })

## ----------------------------------------------------------------------
## Clean up

delete(dat)
db.disconnect(cid, verbose = FALSE)
