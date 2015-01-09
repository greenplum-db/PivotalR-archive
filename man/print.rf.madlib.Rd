\name{print.rf.madlib}
\alias{print.rf.madlib}

\title{
    Print the result of madlib.randomForest
}
\description{
    This function prints the result of \code{\link{madlib.randomForest}} to the screen. It
    internally calls R's \code{\link{print.randomForest}} function.
}

\usage{

    \method{print}{rf.madlib}(x, digits = max(3L, getOption("digits") - 3L),
        ...)

}

\references{ [1] Documentation of decision
    tree in MADlib 1.6, \url{http://doc.madlib.net/latest/} } \author{ Author:
    Predictive Analytics Team at Pivotal Inc.

  Maintainer: Caleb Welton, Pivotal Inc. \email{cwelton@pivotal.io}
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
