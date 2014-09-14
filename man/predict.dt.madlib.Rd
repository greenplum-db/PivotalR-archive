\name{predict.dt.madlib}
\alias{predict.dt.madlib}

\title{
    Compute the predictions of the model produced by madlib.rpart
}
\description{
    This is actually a wrapper for MADlib's predict function of
    decision tree. It accepts the result of \code{\link{madlib.rpart}},
    which is a representation of decision tree, and compute the
    predictions for new data sets.
}

\usage{

    \method{predict}{dt.madlib}(object, newdata, type = c("response", "prob"),
    ...)

}

\arguments{
  \item{object}{
    A \code{dt.madlib} object, which is the result of \code{\link{madlib.rpart}}.
}
  \item{newdata}{
    A \code{\linkS4class{db.obj}} object, which contains the data used for prediction.
    If it is not given, then the data set used to train the model will be used.
}
  \item{type}{
    A string, default is "response". For regessions, this will generate the fitting
    values. For classification, this will generate the predicted class values. There
    is an extra option "prob" for classification tree, which computes the probabilities
    of each class.
}

\item{...}{

    Other arguments. Not implemented yet.

}

}

\value{
    A \code{\linkS4class{db.obj}} object, which wraps a table that contains the predicted
    values and also a valid ID column. For \code{type='response'}, the predicted column
    has the fitted value (regression tree) or the predicted classes (classification tree).
    For \code{type='prob'}, there are one column for each class, which contains the probabilities
    for that class.
}
\references{
[1] Documentation of decision tree in MADlib 1.6, \url{http://doc.madlib.net/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@pivotal.io}
}

\seealso{
    \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, \code{\link{madlib.rpart}},
    \code{\link{madlib.summary}}, \code{\link{madlib.arima}}, \code{\link{madlib.elnet}}
    are all MADlib wrapper functions.

    \code{\link{predict.lm.madlib}}, \code{\link{predict.logregr.madlib}},
    \code{\link{predict.elnet.madlib}}, \code{\link{predict.arima.css.madlib}} are all predict functions
    related to MADlib wrapper functions.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

key(x) <- "id"
fit <- madlib.rpart(rings < 10 ~ length + diameter + height + whole + shell,
       data=x, parms = list(split='gini'), control = list(cp=0.005))

predict(fit, x, 'r')

db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ madlib }
\keyword{ stats }
\keyword{math}
