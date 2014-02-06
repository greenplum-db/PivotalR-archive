context("Example tests which shows how to write tests")

## ----------------------------------------------------------------------
## Test preparations

## Need valid 'port' and 'dbname' values
## This function will get these parameters from the user's
## interactive inputs if they have not been defined.
.get.param.inputs(c("port", "dbname"))

## connection ID
cid <- db.connect(port = port, dbname = dbname, verbose = FALSE)

## data in the datbase
dat.db <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## data in the memory
dat.mm <- abalone

## ----------------------------------------------------------------------
## Tests

test_that("madlib.lm is fast enough", {
    expect_that(madlib.lm(rings ~ . - id - sex, data = dat.db), takes_less_than(2))
})

test_that("madlib.lm returns the correct values", {
    ##
    fdb <- madlib.lm(rings ~ . - id - sex, data = dat.db)
    fm <- summary(lm(rings ~ . - id - sex, data = dat.mm))
    ##
    expect_that(fdb,                    is_a("lm.madlib"))
    expect_that(fdb$data,               is_a("db.data.frame"))
    expect_that(fdb$coef,               equals(as.numeric(fm$coefficients[,1])))
    expect_that(fdb$std_err,            is_equivalent_to(fm$coefficients[,2]))
    expect_that("id" %in% fdb$col.name, is_false())
    expect_that(fdb$has.intercept,      is_true())
    expect_that(fdb$r2,                 is_identical_to(fm$r.squared))
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
