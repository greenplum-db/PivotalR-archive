\name{madlib.randomForest}
\alias{madlib.randomForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MADlib wrapper function for Random Forest
}
\description{
    This function is a wrapper of MADlib's random forest model training
    function. The resulting forest is stored in a table in the database, and one
    can also view the result from R using \code{\link{print.rf.madlib}}.
}
\usage{
madlib.randomForest(formula, data, id = NULL, ntree = 100, mtry = NULL,
importance = FALSE, nPerm = 1, na.action = NULL, control,
na.as.level = FALSE, verbose = FALSE, ...) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
      A formula object, intercept term will automatically be removed. Factors will
      not be expanded to their dummy variables. Grouping syntax is also supported,
      see \code{\link{madlib.lm}} and \code{\link{madlib.glm}} for more details.
}
  \item{data}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the database.
}
  \item{id}{
    A string, the index for each row. If \code{\link{key}} has been specified for
    \code{data}, the key will be used as the ID unless this argument is also specified.
    We have to have this specified so that \code{\link{predict.rf.madlib}}'s result can
    be compared with the original data.
}
  \item{ntree}{
    An integer, maximum number of trees to grow in the random forest model, default is
    100.
}
  \item{mtry}{
    An integer, number of features randomly selected for each split.
}
  \item{importance}{
    A boolean, whether or not to calculate variable importance, default is FALSE.
}
  \item{nPerm}{
    An integer, number of times to permute each feature value while calculating
    variable importance, default is 1.
}
  \item{na.action}{
    A function, which filters the \code{NULL} values from the data. Not implemented yet.
}
  \item{control}{
    A list, which includes parameters for the fit. Supported parameters include:
    'minsplit' - minimum number of observations that must be present in a node for a
    split to be attempted. default is minsplit=20

    'minbucket' - Minimum number of observations in any terminal node,
    default is min_split/3

    'maxdepth' - Maximum depth of any node, default is maxdepth=10

    'nbins' - Number of bins to find possible node split threshold values for continuous
               variables, default is 100 (Must be greater than 1)

    'max_surrogates' - Number of surrogate splits at each node in the trees constructed.

}
  \item{na.as.level}{
    A boolean, indicating if NULL value for a categorical variable is treated as a
    distinct level, default is na.as.level=false
}
  \item{verbose}{
    A boolean, indicating whether or not to print more info, default is verbose=false
}
  \item{\dots}{
    Arguments to be passed to or from other methods.
}
}
\value{
  An S3 object of type rf.madlib in the case of non-grouping, and of type
  rf.madlib.grp in the case of grouping.
}
\references{
[1] Documentation of random forest in MADlib 1.7, \url{http://doc.madlib.net/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
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

## Another example, using grouping
fit <- madlib.randomForest(rings < 10 ~ length + diameter + height + whole + shell | sex,
       data=x)
fit

db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
