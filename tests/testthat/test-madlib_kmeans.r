context("Test cases for madlib.kmeans and its helper functions")

## ------------------------------------------------------------
## Test preparations
env <- new.env(parent = globalenv())
.dbname = get('pivotalr_dbname', envir=env)
.port = get('pivotalr_port', envir=env)
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

dat.matrix <- matrix( c(
    1,14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065,
    2,13.2,1.78,2.14,11.2,1,2.65,2.76,0.26,1.28,4.38,1.05,3.49,1050,
    3,13.16,2.36,2.67,18.6,101,2.8,3.24,0.3,2.81,5.6799,1.03,3.17,1185,
    4,14.37,1.95,2.5,16.8,113,3.85,3.49,0.24,2.18,7.8,0.86,3.45,1480,
    5,13.24,2.59,2.87,21,118,2.8,2.69,0.39,1.82,4.32,1.04,2.93,735,
    6,14.2,1.76,2.45,15.2,112,3.27,3.39,0.34,1.97,6.75,1.05,2.85,1450,
    7,14.39,1.87,2.45,14.6,96,2.5,2.52,0.3,1.98,5.25,1.02,3.58,1290,
    8,14.06,2.15,2.61,17.6,121,2.6,2.51,0.31,1.25,5.05,1.06,3.58,1295,
    9,14.83,1.64,2.17,14,97,2.8,2.98,0.29,1.98,5.2,1.08,2.85,1045,
    10,13.86,1.35,2.27,16,98,2.98,3.15,0.22,1.85,7.2199,1.01,3.55,1045),
    byrow=T, nrow=10)
cols <- c("pid", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10",
    "p11", "p12", "p13")
colnames(dat.matrix) <- cols

cent.r <- matrix(
    c(14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065,
       13.2,1.78,2.14,11.2,1,2.65,2.76,0.26,1.28,4.38,1.05,3.49,1050),
    byrow=T, nrow=2)
colnames(cent.r) <- cols[2:14]


dat <- as.db.data.frame(as.data.frame(dat.matrix), conn.id=cid, verbose=FALSE)
cent <- as.db.data.frame(as.data.frame(cent.r), conn.id=cid, verbose=FALSE)

test_that("Test kmeans (random seed)", {
    testthat::skip_on_cran()
    db.out <- madlib.kmeans(dat, 2, key= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

test_that("Test kmeans (kmeans++)", {
    testthat::skip_on_cran()
	  db.out <- madlib.kmeans(dat, 2, key= 'pid', kmeanspp=TRUE)
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))
})

test_that("Test kmeans (seed table)", {
    testthat::skip_on_cran()
	  db.out <- madlib.kmeans(dat, centers= cent, key= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

test_that("Test kmeans (seed matrix)", {
    testthat::skip_on_cran()
	  db.out <- madlib.kmeans(dat, cent.r, key= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

db.disconnect(cid, verbose = FALSE)
