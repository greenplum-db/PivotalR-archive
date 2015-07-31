context("Test cases for madlib.lda and its helper functions")

## ------------------------------------------------------------
## Test preparations

.get.param.inputs(c(".port", ".dbname"))
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
dat <- as.db.data.frame(documents, conn.id = cid, verbose = FALSE)
dat.r <- documents

# test_that("Test voc_size", {
#           fit.db <- madlib.lm(rings ~ . - id - sex, data = dat, control = list(max.iter = 20, use.lm = T))
#           fit.r <- summary(lm(rings ~ . - id - sex, data = dat.r))

#           expect_equal(fit.db$coef, fit.r$coefficients[ , 1], tolerance=1e-2, check.attributes=FALSE)
#           expect_equal(fit.db$std_err, fit.r$coefficients[ , 2], tolerance=1e-2, check.attributes=FALSE)
# })

## Fit a model (from demo(lda)).
# data(cora.documents)
# data(cora.vocab)
# K <- 10 ## Num clusters
# result <- lda.collapsed.gibbs.sampler(cora.documents,
# K, ## Num clusters
# cora.vocab,
# 25, ## Num iterations
# 0.1,
# 0.1)
# ## Predict new words for the first two documents
# predictions <- predictive.distribution(result$document_sums[,1:2],
# result$topics,
# 0.1, 0.1)
# ## Use top.topic.words to show the top 5 predictions in each document.
# top.topic.words(t(predictions), 5)
## [,1] [,2]
## [1,] "learning" "learning"
## [2,] "algorithm" "paper"
## [3,] "model" "problem"
## [4,] "paper" "results"
## [5,] "algorithms" "system"