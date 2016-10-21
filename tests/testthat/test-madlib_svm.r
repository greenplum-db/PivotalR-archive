context("Test cases for madlib.svm and its helper functions")

test_that("Test kernel params parsing", {
  kernel.str <- "gaussiaN"
  degree <- 3
  gamma <- 0.5
  coef0 <- 2
  kernel <- .validate.kernel(kernel.str, degree, gamma, coef0, list(n.components=40))
  expect_equal(kernel$func, "gaussian")
  expect_equal(kernel$params, "n_components = 40, gamma = 0.5")

  kernel.str <- "polynomial"
  kernel <- .validate.kernel(kernel.str, degree, gamma, coef0, list(n.components=40))
  expect_equal(kernel$func, "polynomial")
  expect_equal(kernel$params, "n_components = 40, degree = 3, coef0 = 2")

  kernel <- .validate.kernel(kernel.str, degree, gamma, coef0, list(fit.intercept=TRUE))
  expect_equal(kernel$params, "fit_intercept = TRUE, degree = 3, coef0 = 2")
})

test_that("Test control params parsing", {
  cross = 4
  class.weight = NULL
  tolerance = 1e-10
  lambda = c(0.1, 1)
  epsilon = c(1, 2)
  control = list(max.iter = c(1, 100), norm = "l2", decay.factor = c(3, 7))
  params <- .validate.params(class.weight, tolerance, epsilon, cross, lambda, control)
  expect_equal(params,
               paste0("tolerance = 1e-10, lambda = [0.1, 1], n_folds = 4, ",
                      "epsilon = [1, 2], max_iter = [1, 100], norm = l2, ",
                      "decay_factor = [3, 7]"))

  epsilon <- NULL
  control <- list()
  params <- .validate.params(class.weight, tolerance, epsilon, cross, lambda, control)
  expect_equal(params, "tolerance = 1e-10, lambda = [0.1, 1], n_folds = 4")
})

# ------------------------------------------------------------------------------
# -- Tests for the database functions ------------------------------------------
# ------------------------------------------------------------------------------
env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)

conn.id <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
data <- as.db.data.frame(abalone, conn.id = conn.id, verbose = FALSE)
data.table <- content(data)
madlib <- schema.madlib(conn.id)

test_that("Test classification with kernel", {
    testthat::skip_on_cran()
    fit.svm <- madlib.svm(rings > 7 ~ . - id - sex, data = data, type = "classification",
                          kernel = "gaussian",  control = list(max.iter=3))
    model.sql.name <- .unique.string()
    sql <- sprintf("select %s.svm_classification('%s', '%s', '(\"rings\") > (7)',
                   'array[1,\"length\",\"diameter\",\"height\",\"whole\",\"shucked\",\"viscera\",\"shell\"]',
                   'gaussian', '', '', 'tolerance = 1e-10, lambda = 0.01, n_folds = 0, max_iter = 3', FALSE);",
                   madlib, data.table, model.sql.name)
    sql <- paste(sql, ";", "select * from ", model.sql.name)
    model <- db.q(sql, conn.id = conn.id, verbose = FALSE)

    pred.r <- predict(fit.svm, newdata = data, id.col = "id")
    pred.r <- lookat(pred.r, -1)
    pred.sql.name <- .unique.string()
    sql <- sprintf("select %s.svm_predict('%s', '%s', 'id', '%s')",
                   madlib, model.sql.name, data.table, pred.sql.name)
    sql <- paste(sql, ";", "select * from ", pred.sql.name)
    pred.sql <- db.q(sql, conn.id = conn.id, nrows = -1, verbose = FALSE)

    res.sql <- pred.sql[with(pred.sql, order(id)), ]$prediction
    res.r <- pred.r[with(pred.r, order(id)), ]$prediction
    expect_equal(res.sql, res.r, tolerance = 1e-2)
    })

test_that("Test regression with group", {
    testthat::skip_on_cran()
    fit.svm <- madlib.svm(rings ~ height + shell | sex , data = data, type = "regression",
                          kernel = "linear", cross = 0, lambda = 0.05,
                          control = list(max.iter=3))
    model.sql <- .unique.string()
    sql <- sprintf("select %s.svm_regression('%s', '%s', '\"rings\"', 'array[1,\"height\",\"shell\"]',
                   'linear', '', 'sex', 'tolerance = 1e-10, lambda = 0.05, n_folds = 0, max_iter = 3', FALSE);",
                   madlib, data.table, model.sql)
    sql <- paste(sql, ";", "select * from ", model.sql)
    model <- db.q(sql, conn.id = conn.id, verbose = FALSE)

  # sort model by sex
  model <- model[with(model, order(sex)), ]
  # sort fit.svm by sex
  fit.svm.ordered <- fit.svm[order(sapply(fit.svm, function(x) x[["sex"]]))]

  coef.sql <- arraydb.to.arrayr(model$coef)
  coef.r <- t(sapply(fit.svm.ordered, function (x) x[["coef"]]))
  expect_equal(coef.r, coef.sql, tolerance = 1e-2)

  pred.r <- predict(fit.svm, newdata = data, id.col = "id")
  pred.r <- lookat(pred.r, -1)
  pred.sql.name <- .unique.string()
  sql <- sprintf("select %s.svm_predict('%s', '%s', 'id', '%s')",
                 madlib, model.sql, data.table, pred.sql.name)
  sql <- paste(sql, ";", "select * from ", pred.sql.name)
  pred.sql <- db.q(sql, conn.id = conn.id, nrows = -1, verbose = FALSE)

  res.sql <- pred.sql[with(pred.sql, order(id)), ]$prediction
  res.r <- pred.r[with(pred.r, order(id)), ]$prediction
  expect_equal(res.sql, res.r, tolerance = 1e-2)
})

test_that("Test one-class svm", {
    testthat::skip_on_cran()
    fit.svm <- madlib.svm(~ height + shell, data = data, type = "one-class", control = list(max.iter=3))
    model.sql <- .unique.string()
    sql <- sprintf("select %s.svm_one_class('%s', '%s', 'array[1,\"height\",\"shell\"]',
                   'gaussian', '', '', 'tolerance = 1e-10, lambda = 0.01, n_folds = 0, class_weight = balanced, max_iter = 3', FALSE);",
                   madlib, data.table, model.sql);
    sql <- paste(sql, ";", "select * from ", model.sql)
    model <- db.q(sql, conn.id = conn.id, verbose = FALSE)

    coef.sql <- arraydb.to.arrayr(model$coef)
    coef.r <- matrix(fit.svm$coef, byrow = TRUE, nrow = 1)
    expect_equal(coef.r, coef.sql, tolerance = 1e-2)

    pred.r <- predict(fit.svm, newdata = data, id.col = "id")
    pred.r <- lookat(pred.r, -1)
    pred.sql.name <- .unique.string()
    sql <- sprintf("select %s.svm_predict('%s', '%s', 'id', '%s')",
                   madlib, model.sql, data.table, pred.sql.name)
    sql <- paste(sql, ";", "select * from ", pred.sql.name)
    pred.sql <- db.q(sql, conn.id = conn.id, nrows = -1, verbose = FALSE)

    res.sql <- pred.sql[with(pred.sql, order(id)), ]$prediction
    res.r <- pred.r[with(pred.r, order(id)), ]$prediction
    expect_equal(res.sql, res.r, tolerance = 1e-2)
})
