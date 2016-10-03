context("Test cases for madlib.kmeans and its helper functions")

## ------------------------------------------------------------
## Test preparations

.get.param.inputs(c(".port", ".dbname"))
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db.q("DROP TABLE IF EXISTS __madlib_km_sample__",
        nrows = -1, conn.id = cid, verbose = FALSE)
db.q("CREATE TABLE __madlib_km_sample__(pid int, points double precision[])",
        nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('1' , '{14.23, 1.71, 2.43, 15.6, 127, 2.8, 3.0600, 0.2800, 2.29, 5.64, 1.04, 3.92, 1065}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('2' , '{13.2, 1.78, 2.14, 11.2, 1, 2.65, 2.76, 0.26, 1.28, 4.38, 1.05, 3.49, 1050}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('3' , '{13.16, 2.36,  2.67, 18.6, 101, 2.8,  3.24, 0.3, 2.81, 5.6799, 1.03, 3.17, 1185}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('4' , '{14.37, 1.95, 2.5, 16.8, 113, 3.85, 3.49, 0.24, 2.18, 7.8, 0.86, 3.45, 1480}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('5' , '{13.24, 2.59, 2.87, 21, 118, 2.8, 2.69, 0.39, 1.82, 4.32, 1.04, 2.93, 735}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('6' , '{14.2, 1.76, 2.45, 15.2, 112, 3.27, 3.39, 0.34, 1.97, 6.75, 1.05, 2.85, 1450}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('7' , '{14.39, 1.87, 2.45, 14.6, 96, 2.5, 2.52, 0.3, 1.98, 5.25, 1.02, 3.58, 1290}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('8' , '{14.06, 2.15, 2.61, 17.6, 121, 2.6, 2.51, 0.31, 1.25, 5.05, 1.06, 3.58, 1295}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('9' , '{14.83, 1.64, 2.17, 14, 97, 2.8, 2.98, 0.29, 1.98, 5.2, 1.08, 2.85, 1045}')",  nrows = -1, conn.id = cid, verbose = FALSE)
db.q("INSERT INTO __madlib_km_sample__ VALUES ('10' , '{13.86, 1.35, 2.27, 16, 98, 2.98, 3.15, 0.22, 1.8500, 7.2199, 1.01, 3.55, 1045}')",  nrows = -1, conn.id = cid, verbose = FALSE)


db.q("DROP TABLE IF EXISTS __madlib_km_centroids__",
        nrows = -1, conn.id = cid, verbose = FALSE)
db.q("CREATE TABLE __madlib_km_centroids__ AS
SELECT points
FROM __madlib_km_sample__
ORDER BY random()
LIMIT 2", nrows = -1, conn.id = cid, verbose = FALSE)

dat <- db.data.frame("__madlib_km_sample__", conn.id = cid, verbose = FALSE)
cent <- db.data.frame("__madlib_km_centroids__", conn.id = cid, verbose = FALSE)

seed.matrix <- matrix(
	c(14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065,
		13.2,1.78,2.14,11.2,1,2.65,2.76,0.26,1.28,4.38,1.05,3.49,1050),
	byrow=T, nrow=2)

test_that("Test kmeans (random seed)", {

	db.out <- madlib.kmeans(dat, 2, pid= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

test_that("Test kmeans (kmeans++)", {

	db.out <- madlib.kmeans(dat, 2, pid= 'pid', kmeanspp=TRUE)
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))
})

test_that("Test kmeans (seed table)", {

	db.out <- madlib.kmeans(dat, centers= cent, pid= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

test_that("Test kmeans (seed matrix)", {

	db.out <- madlib.kmeans(dat, seed.matrix, pid= 'pid')
    expect_that(db.out, is_a("kmeans"))
    expect_that(db.out$centers, is_a("matrix"))

})

db.q("DROP TABLE IF EXISTS __madlib_km_sample__",
        nrows = -1, conn.id = cid, verbose = FALSE)

db.q("DROP TABLE IF EXISTS __madlib_km_centroids__",
        nrows = -1, conn.id = cid, verbose = FALSE)

db.disconnect(cid, verbose = FALSE)
