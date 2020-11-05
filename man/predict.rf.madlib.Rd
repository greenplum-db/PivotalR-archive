\name{predict.rf.madlib}
\alias{predict.rf.madlib}

\title{
    Compute the predictions of the model produced by madlib.randomForest
}
\description{
    This is actually a wrapper for MADlib's predict function of
    random forests. It accepts the result of \code{\link{madlib.randomForest}},
    which is a representation of a random forest model, and computes the
    predictions for new data sets.
}

\usage{

    \method{predict}{rf.madlib}(object, newdata, type = c("response", "prob"),
    ...)

}

\arguments{
  \item{object}{
    A \code{rf.madlib} object, which is the result of \code{\link{madlib.randomForest}}.
}
  \item{newdata}{
    A \code{\linkS4class{db.obj}} object, which contains the data used for prediction.
    If it is not given, then the data set used to train the model will be used.
}
  \item{type}{
    A string, default is "response". For regessions, this will generate the fitting
    values. For classification, this will generate the predicted class values. There
    is an extra option "prob" for classification models, which computes the probabilities
    of each class.
}

\item{...}{

    Other arguments. Not implemented yet.

}

}

\value{
    A \code{\linkS4class{db.obj}} object, which wraps a table that contains the predicted
    values and also a valid ID column. For \code{type='response'}, the predicted column
    has the fitted value (regression model) or the predicted classes (classification model).
    For \code{type='prob'}, there are one column for each class, which contains the probabilities
    for that class.
}
\references{
[1] Documentation of random forests in MADlib 1.7, \url{https://madlib.apache.org/docs/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
    \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, \code{\link{madlib.randomForest}},
    \code{\link{madlib.rpart}}, \code{\link{madlib.summary}}, \code{\link{madlib.arima}},
    \code{\link{madlib.elnet}}
    are all MADlib wrapper functions.

    \code{\link{predict.lm.madlib}}, \code{\link{predict.logregr.madlib}},
    \code{\link{predict.elnet.madlib}}, \code{\link{predict.arima.css.madlib}},
    \code{\link{predict.dt.madlib}}, \code{\link{predict.rf.madlib}}
    are all predict functions related to MADlib wrapper functions.
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
fit <- madlib.randomForest(rings < 10 ~ length + diameter + height + whole + shell,
       data=x)

predict(fit, x, 'r')

db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ madlib }
\keyword{ stats }
\keyword{math}
