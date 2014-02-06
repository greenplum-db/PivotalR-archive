context("Example tests which shows how to write tests")

## ----------------------------------------------------------------------
## Test preparations

## Need valid 'port' and 'dbname' values
## It is not necessary to run the check, but it is a good reminder to
## the writer of the tests that these environment variables are needed
## by these tests.
.get.param.inputs(c("port", "dbname"))

## connection ID
cid <- db.connect(port = port, dbname = dbname, verbose = FALSE)

## data in the datbase
dat.db <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## data in the memory
dat.mm <- abalone

## ----------------------------------------------------------------------
## Tests

test_that("madlib.lm returns the correct class", {
    ##
    fdb <- madlib.lm(rings ~ . - id - sex, data = dat.db)
    fm <- lm(rings ~ . - id - sex, data = dat.mm)
    ##
    expect_that(fdb, is_a("lm.madlib"))
    expect_that(fdb$coef, equals(fm$coefficients))
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
