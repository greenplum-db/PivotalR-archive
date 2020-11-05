\name{getTree.rf.madlib}
\alias{getTree.rf.madlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MADlib wrapper function for Random Forest
}
\description{
    This function is a wrapper of MADlib's random forest model get_tree
    function. The model built using \code{\link{madlib.randomForest}} is passed
    as input to this function.
}
\usage{
getTree.rf.madlib(object, k=1, ...) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
      A random forest model object built using \code{\link{madlib.randomForest}}.
}
  \item{k}{
    Id of the tree to be retrieved. Can range between 1 and maximum number of trees
    in the forest. default is 1.
}
  \item{\dots}{
      Arguments to be passed to or from other methods.
}
}
\value{
  A data frame object similar to R's getTree result.
}
\references{
[1] Documentation of random forest in MADlib 1.7, \url{https://madlib.apache.org/docs/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
  \code{\link{madlib.randomForest}} function to train a random forest model.

  \code{\link{print.rf.madlib}} function to print summary of a model fitted
  through madlib.randomForest

  \code{\link{predict.rf.madlib}} is a wrapper for MADlib's predict function for
  random forests.

  \code{\link{madlib.lm}}, \code{\link{madlib.glm}},
        \code{\link{madlib.summary}}, \code{\link{madlib.arima}}, \code{\link{madlib.elnet}},
        \code{\link{madlib.rpart}} are all MADlib wrapper functions.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

## decision tree using abalone data, using default values of minsplit,
## maxdepth etc.
key(x) <- "id"
fit <- madlib.randomForest(rings < 10 ~ length + diameter + height + whole + shell,
       data=x)
fit
getTree.rf.madlib(fit, k=2)


db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{'tree'}
