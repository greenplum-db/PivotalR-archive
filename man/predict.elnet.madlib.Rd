\name{predict.elnet.madlib}
\alias{predict.elnet.madlib}

\title{
  Predict using the regression result of elastic net regularization
}
\description{
  Prediction from models fit by \code{\link{madlib.elnet}}
}
\usage{
\method{predict}{elnet.madlib}(object, newdata, type = c("response", "prob"), ...)
}

\arguments{
  \item{object}{
    The result of \code{\link{madlib.elnet}}
}
  \item{newdata}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the
    database.
}
  \item{type}{
    A string, default is "response". The other option is "prob". The prediction for "gaussian"/"linear" family of \code{\link{madlib.elnet}} result is always for the dependent variable. The prediction for "binomial"/"logistic" family is \code{TRUE/FALSE} values for "response", and probabilities of \code{TRUE} for "prob".
}
  \item{\dots}{
    Extra parameters. Not implemented yet.
}
}

\value{
 A \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  to compute the prediction. One can use the function \code{\link{lk}} to look at the values.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{madlib.elnet}} Wrapper for MADlib elastic net     regularization.

  \code{\link{predict.lm.madlib}} and
  \code{\link{predict.logregr.madlib}} produce predictions for linear
  and logistic models.
}

\examples{
## see the examples in madlib.elnet
}

\keyword{madlib}
\keyword{stats}
\keyword{math}
