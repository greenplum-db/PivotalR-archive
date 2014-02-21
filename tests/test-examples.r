context("Examples that show how to write tests")

## ----------------------------------------------------------------------
## Test preparations

## Need valid 'port' and 'dbname' values
## This function will get these parameters from the user's
## interactive inputs if they have not been defined.
.get.param.inputs(c(".port", ".dbname"))

## connection ID
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## data in the datbase
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## data in the memory
dat.im <- abalone

## ----------------------------------------------------------------------
## Tests

test_that("Examples of speed test",
          ## takes les than 2 seconds
          ## 1
          expect_this(madlib.lm(rings ~ . - id - sex, data = dat),
                      takes_less_than(3)))

## ----------------------------------------------------------------------

test_that("Examples of class attributes", {
    ## do some calculation inside test_that
    ## These values are not avilable outside test_that function
    fdb <- madlib.lm(rings ~ . - id - sex, data = dat)
    fm <- summary(lm(rings ~ . - id - sex, data = dat.im))
    ## 2, 3
    expect_this(fdb,      is_a("lm.madlib"))
    expect_this(fdb$data, is_a("db.data.frame"))
})

## ----------------------------------------------------------------------
## To make the computation results available to later test_that
## need to do the calculation on the upper level
## ----------------------------------------------------------------------

fdb <- madlib.lm(rings ~ . - id - sex, data = dat)
fm <- summary(lm(rings ~ . - id - sex, data = dat.im))

test_that("Examples of value equivalent", {
    ## numeric values are the same, but names are not
    ## 4, 5, 6
    expect_this(fdb$coef,    equals(as.numeric(fm$coefficients[,1])))
    expect_this(fdb$coef,    is_equivalent_to(fm$coefficients[,1]))
    expect_this(fdb$std_err, is_equivalent_to(fm$coefficients[,2]))
})

## ----------------------------------------------------------------------

test_that("Examples of testing TRUE or FALSE", {
    ## 7, 8
    expect_this("no_such_col" %in% fdb$col.name, is_false())
    expect_this(fdb$has.intercept,               is_true())
})

## ----------------------------------------------------------------------

test_that("Example of identical", {
    ## Two values are equal but not identical
    ## expect_this(fdb$r2, is_identical_to(fm$r.squared)) # will fail

    r2 <- fdb$r2 # same object, identical
    ## 9
    expect_this(fdb$r2, is_identical_to(r2))
})

## ----------------------------------------------------------------------

test_that("Examples of testing string existence", {
    tmp <- dat
    tmp$new.col <- 1
    ## 10, 11
    expect_this(names(tmp), matches("new.col", all = FALSE)) # one value matches
    expect_this(print(tmp), prints_text("temporary"))
})

## ----------------------------------------------------------------------

test_that("Examples of testing errors",
          ## 12
          expect_this(db.q("\\dn", verbose = FALSE, conn.id = cid), # prevent printing un-needed info
                      throws_error("syntax error")))

## ----------------------------------------------------------------------

test_that("Examples of testing warnings",
          ## 13
          expect_this(madlib.elnet(rings ~ . - id, data = dat, method = "cd"),
                      gives_warning("number of features is larger")))

## ----------------------------------------------------------------------

test_that("Examples of testing message",
          ## 14
          expect_this(db.q("select * from", content(dat), conn.id = cid),
                      shows_message("Executing")))

## ----------------------------------------------------------------------
## If you want to use the combinations of multiple
## variables, use 'for' loops
## The following examples show that you can use
## 'for' loops to construct test cases.
## ----------------------------------------------------------------------

test_that("Examples of running tests in loop", {
    rows <- c(1, 5, 10)
    ## 15, 16, 17
    for (n in rows)
        expect_this(nrow(lk(dat, n)), equals(n))
})

## ----------------------------------------------------------------------

test_that("Examples of using multiple loops", {
    fit.this <- "rings ~ height + whole"
    vars <- c("shell", "length", "diameter", "shucked")
    rows <- c(1000, 2000, 3000)
    ## 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
    for (var in vars) {
        fit.this <- paste(fit.this, "+", var)
        for (n in rows)
            expect_this(madlib.lm(formula(fit.this), data = dat[dat$id < n, ]),
                        takes_less_than(3))
    }
})

## ----------------------------------------------------------------------
## Some complicated functions need multiple lines, which can be put inside
## a pair of {}
## ----------------------------------------------------------------------

test_that("Install-check should run without error",
          ## 30
          expect_this({
              x <- matrix(rnorm(100*20),100,20)
              y <- rnorm(100, 0.1, 2)

              dat <- data.frame(x, y) # this data is available only in this block
              delete("eldata", conn.id = cid)
              z <- as.db.data.frame(dat, "eldata", conn.id = cid, verbose = FALSE)

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
          }, has_no_error())) # has_no_error is usually used for doc Examples

## ----------------------------------------------------------------------

test_that("Install-check 2",
          ## 31
          expect_this({
              err <- generic.cv(
                  function(data) {
                      madlib.lm(rings ~ . - id - sex, data = data)
                  },
                  predict,
                  function(predicted, data) {
                      lookat(mean((data$rings - predicted)^2))
                  },
                  data = dat, # this dat is the global dat
                  verbose = FALSE)
          }, has_no_error()))

## ----------------------------------------------------------------------
## Same test, different results on different platforms
## ----------------------------------------------------------------------

test_that("Different results on different platforms",
          ## 32
          expect_this(
              as.character(db.q("select version()", conn.id = cid,
                                verbose = FALSE)),
              if (.get.dbms.str(cid)$db.str == "HAWQ") {
                  matches("HAWQ")
              } else if (.get.dbms.str(cid)$db.str == "PostgreSQL") {
                  matches("PostgreSQL")
              } else {
                  matches("Greenplum")
              }))

## ----------------------------------------------------------------------

test_that("Different results on different versions of HAWQ",
          ## 33
          expect_this(
              as.character(db.q("select madlib.version()", conn.id = cid,
                                verbose = FALSE)),
          {
              db <- .get.dbms.str(cid)
              if (db$db.str == "HAWQ") {
                  if (grepl("^1\\.1", db$version.str)) # older HAWQ
                      matches("0\\.5")
                  else # new HAWQ
                      matches("MADlib version: 1\\.")
              } else {
                  matches("MADlib") # always pass
              }
          }))

## ----------------------------------------------------------------------
## Skip tests
## ----------------------------------------------------------------------

## skip on all situations
skip_if(TRUE, {
    ## 34, 35, 36
    test_that("Always skip this", {
        tmp <- dat
        tmp$new.col <- 1
        ##
        expect_this(names(tmp), matches("new.col", all = FALSE))
        expect_this(print(tmp), prints_text("temporary"))
    })

    test_that("Also always skip this",
              expect_this(db.q("\\dn", verbose = FALSE),
                          throws_error("syntax error")))
})

## ----------------------------------------------------------------------

db <- .get.dbms.str(cid)

skip_if(db$db.str %in% c("HAWQ", "PostgreSQL"),
        test_that("Skip this on HAWQ",
                  ## 37
                  expect_this(db.q("\\dn", verbose = FALSE),
                              throws_error("syntax error"))))

test_that("Skip some tests", {
    tmp <- dat
    tmp$new.col <- 1
    ## 38, 39
    skip_if(db$db.str == "HAWQ" && grepl("^1\\.2", db$version.str),
            expect_this(names(tmp), matches("new.col", all = FALSE)))
    expect_this(print(tmp), prints_text("temporary"))
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
