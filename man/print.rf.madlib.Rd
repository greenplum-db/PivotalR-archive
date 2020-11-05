\name{print.rf.madlib}
\alias{print.rf.madlib}

\title{
    Print the result of madlib.randomForest
}
\description{
    This function prints the result of \code{\link{madlib.randomForest}} to the screen. It
    internally calls R's print function for random forests.
}

\usage{

    \method{print}{rf.madlib}(x, digits = max(3L, getOption("digits") - 3L),
        ...)

}

\arguments{
    \item{x}{ The fitted forest from the result of
        \code{\link{madlib.randomForest}} }
    \item{digits}{ The number of digits to
        print for numerical values. }
    \item{\dots}{ Arguments to be passed to
        or from other methods. } }
\references{ [1] Documentation of random
    forest in MADlib 1.7, \url{https://madlib.apache.org/docs/latest/} }
\author{ Author:
    Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}


\seealso{
  \code{\link{madlib.randomForest}} is the wrapper for MADlib's forest_train function for random forests.

  \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, \code{\link{madlib.rpart}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}}, \code{\link{madlib.elnet}},
  \code{\link{madlib.rpart}}
  are all MADlib wrapper functions.
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

## random forest using abalone data, using default values of minsplit,
## maxdepth etc.
key(x)<-"id"
fit <- madlib.randomForest(rings < 10 ~ length + diameter + height + whole + shell,
       data=x)

print(fit)

db.disconnect(cid)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{'tree'}
