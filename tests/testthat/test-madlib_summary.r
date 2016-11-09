context("Test cases for madlib.summary")

## ------------------------------------------------------------
## Test preparations

env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db <- .get.dbms.str(cid)
dat <- as.db.data.frame(abalone)

test_that("Test summary", {

	ml.sum <- madlib.summary(dat)
	r.sum <- summary(abalone)

	expect_equal(ml.sum$third_quartile[10],
		as.numeric(unlist(strsplit(r.sum[5,10],":"))[2]),
		tolerance=1e-1, check.attributes=FALSE )

})
