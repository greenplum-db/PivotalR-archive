context("Test cases for madlib.lda and its helper functions")

## ------------------------------------------------------------
## Test preparations

#.get.param.inputs(c(".port", ".dbname"))
cid <- db.connect(port = 5431, dbname = "madlib-pg93", verbose = FALSE)
db <- .get.dbms.str(cid)

library(topicmodels)
data("AssociatedPress", package = "topicmodels")
temp_dat <- AssociatedPress
dat1 <- cbind(temp_dat$i,temp_dat$j,temp_dat$v)
dat1 <- as.data.frame(dat1)
colnames(dat1) <- c("docid", "wordid", "count")
dat2 <- as.data.frame(temp_dat$dimnames[2]$Terms)
dat2 <- cbind(1:nrow(dat2),dat2)
colnames(dat2) <- c("wordid", "word")

termfreq <- .unique.string()
vocab <- .unique.string()
newdata <- .unique.string()

dat1 <- as.db.data.frame(dat1, conn.id=cid, verbose=FALSE, is.temp=FALSE, table.name=termfreq)
dat2 <- as.db.data.frame(dat2, conn.id=cid, verbose=FALSE, is.temp=FALSE, table.name=vocab)

sql <- paste("DROP TABLE IF EXISTS ", newdata, "; CREATE TABLE ", newdata, 
    " AS SELECT docid, array_agg(word) AS words FROM (SELECT *, generate_series(1,count::int) FROM ",
        termfreq,  " JOIN ", vocab, " USING (wordid)) subq GROUP BY docid;", sep="")
db.q(sql, verbose=FALSE)

dat.r <- AssociatedPress

dat <- db.data.frame(newdata, conn.id=cid)

test_that("Test perplexity", {
           output.db <- madlib.lda(dat, "docid","words",10,0.1,0.1, 10)
           output.r <- LDA(AssociatedPress, k=10,
           control=list(iter=10, alpha=0.1, delta=0.1),method="Gibbs")

           perplexity.db <- perplexity.lda.madlib(output.db)
           perplexity.r <- perplexity(output.r, newdata=AssociatedPress)

           expect_equal(perplexity.db, perplexity.r, tolerance=1e-2, check.attributes=FALSE)
           
 })

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