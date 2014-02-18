context("Examples that show how to write tests")

## ----------------------------------------------------------------------
## Test preparations

## Need valid 'port' and 'dbname' values
## This function will get these parameters from the user's
## interactive inputs if they have not been defined.
.get.param.inputs(c(".port", ".dbname"))

## connection ID
cid <- db.connect(port = port, dbname = dbname, verbose = FALSE)

## data in the datbase
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## data in the memory
dat.im <- abalone

## ----------------------------------------------------------------------
## Tests

test_that("Examples of speed test", {
    # takes les than 2 seconds
    expect_that(madlib.lm(rings ~ . - id - sex, data = dat),
                takes_less_than(3))
})

##

test_that("Examples of class attributes", {
    ## do some calculation inside test_that
    ## These values are not avilable outside test_that function
    fdb <- madlib.lm(rings ~ . - id - sex, data = dat)
    fm <- summary(lm(rings ~ . - id - sex, data = dat.im))
    ##
    expect_that(fdb,      is_a("lm.madlib"))
    expect_that(fdb$data, is_a("db.data.frame"))
})

## To make the computation results available to later test_that
## need to do the calculation on the upper level
fdb <- madlib.lm(rings ~ . - id - sex, data = dat)
fm <- summary(lm(rings ~ . - id - sex, data = dat.im))

test_that("Examples of value equivalent", {
    ## numeric values are the same, but names are not
    expect_that(fdb$coef,    equals(as.numeric(fm$coefficients[,1])))
    expect_that(fdb$coef,    is_equivalent_to(fm$coefficients[,1]))
    expect_that(fdb$std_err, is_equivalent_to(fm$coefficients[,2]))
})

##

test_that("Examples of testing TRUE or FALSE", {
    expect_that("no_such_col" %in% fdb$col.name, is_false())
    expect_that(fdb$has.intercept,               is_true())
})

##

test_that("Example of identical", {
    ## Two values are equal but not identical
    ## expect_that(fdb$r2, is_identical_to(fm$r.squared)) # will fail

    r2 <- fdb$r2 # same object, identical
    expect_that(fdb$r2, is_identical_to(r2))
})

##

test_that("Examples of testing string existence", {
    tmp <- dat
    tmp$new.col <- 1
    ##
    expect_that(names(tmp), matches("new.col", all = FALSE)) # one value matches
    expect_that(print(tmp), prints_text("temporary"))
})

##

test_that("Examples of testing errors", {
    expect_that(db.q("\\dn", verbose = FALSE), # prevent printing un-needed info
                throws_error("syntax error"))
})

##

test_that("Examples of testing warnings", {
    expect_that(madlib.elnet(rings ~ . - id, data = dat, method = "cd"),
                gives_warning("number of features is larger"))
})

##

test_that("Examples of testing message", {
    expect_that(db.q("select * from", content(dat)),
                shows_message("Executing"))
})

## If you want to use the combinations of multiple
## variables, use 'for' loops
## The following examples show that you can use
## 'for' loops to construct test cases.
test_that("Examples of running tests in loop", {
    rows <- c(1, 5, 10)
    for (n in rows)
        expect_that(nrow(lk(dat, n)), equals(n))
})

##

test_that("Examples of using multiple loops", {
    fit.this <- "rings ~ height + whole"
    vars <- c("shell", "length", "diameter", "shucked")
    rows <- c(1000, 2000, 3000)
    for (var in vars) {
        fit.this <- paste(fit.this, "+", var)
        for (n in rows)
            expect_that(madlib.lm(formula(fit.this), data = dat[dat$id < n, ]),
                        takes_less_than(3))
    }
})

## Some complicated functions need multiple lines, which can be put inside
## a pair of {}

test_that("Install-check should run without error", {
    expect_that(
    {
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
    }, has_no_error())}) # has_no_error is usually used for doc Examples

##

test_that("Install-check 2", {
    expect_that(
    {
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
    }, has_no_error())
})

## ----------------------------------------------------------------------
## Clean up

db.disconnect(cid, verbose = FALSE)
