context("Examples that show how to write tests")

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

test_that("Examples of speed test", {
    # takes les than 2 seconds
    expect_that(madlib.lm(rings ~ . - id - sex, data = dat.db), takes_less_than(3))
})

test_that("Examples of class attributes", {
    ## do some calculation inside test_that
    ## These values are not avilable outside test_that function
    fdb <- madlib.lm(rings ~ . - id - sex, data = dat.db)
    fm <- summary(lm(rings ~ . - id - sex, data = dat.mm))
    ##
    expect_that(fdb,      is_a("lm.madlib"))
    expect_that(fdb$data, is_a("db.data.frame"))
})

## To make the computation results available to later tests
## need to do the calculation on the upper level
fdb <- madlib.lm(rings ~ . - id - sex, data = dat.db)
fm <- summary(lm(rings ~ . - id - sex, data = dat.mm))

test_that("Examples of value equivalent", {
    ## numeric values are the same, but names are not
    expect_that(fdb$coef,    equals(as.numeric(fm$coefficients[,1])))
    expect_that(fdb$coef,    is_equivalent_to(fm$coefficients[,1]))
    expect_that(fdb$std_err, is_equivalent_to(fm$coefficients[,2]))
})

test_that("Examples of testing TRUE or FALSE", {
    expect_that("no_such_col" %in% fdb$col.name, is_false())
    expect_that(fdb$has.intercept,               is_true())
})

test_that("Example of identical", {
    ## Two values are equal but not identical
    ## expect_that(fdb$r2, is_identical_to(fm$r.squared))

    r2 <- fdb$r2 # same object, identical
    expect_that(fdb$r2, is_identical_to(r2))
})

test_that("Examples of testing string existence", {
    tmp <- dat.db
    tmp$new.col <- 1
    ##
    expect_that(names(tmp), matches("new.col", all = FALSE)) # one value matches
    expect_that(print(tmp), prints_text("temporary"))
})

test_that("Examples of testing errors", {
    expect_that(db.q("\\dn", verbose = FALSE), # prevent printing un-needed info
                throws_error("syntax error"))
})

test_that("Examples of testing warnings", {
    expect_that(madlib.elnet(rings ~ . - id, data = dat.db, method = "cd"),
                gives_warning("number of features is larger"))
})

test_that("Examples of testing message", {
    expect_that(db.q("select * from", content(dat.db)),
                shows_message("Executing"))
})

## If you want to use the combinations of multiple
## variables, use 'for' loops
test_that("Examples of running tests in loop", {
    rows <- c(1, 5, 10)
    for (n in rows)
        expect_that(nrow(lk(dat.db, n)), equals(n))
})

test_that("Examples of using multiple loops", {
    fit.this <- "rings ~ height + whole"
    vars <- c("shell", "length", "diameter", "shucked")
    rows <- c(1000, 2000, 3000)
    for (var in vars) {
        fit.this <- paste(fit.this, "+", var)
        for (n in rows)
            expect_that(madlib.lm(formula(fit.this), data = dat.db[dat.db$id < n, ]),
                        takes_less_than(3))
    }
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
