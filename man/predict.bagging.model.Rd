\name{predict.bagging.model}
\alias{predict.bagging.model}

\title{
  Make predictions using the result of \code{\link{generic.bagging}}
}

\description{
  Make predictions using boostrap aggregating models
}
\usage{
\method{predict}{bagging.model}(object, newdata, combine = "mean", ...)
}

\arguments{
  \item{object}{
    A \code{bagging.model}, which is the result of
  \code{\link{generic.bagging}}.
  }
  
  \item{newdata}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the
    database. 
  }
  
  \item{combine}{
    A string, default is \code{"mean"}. The other choice is
    \code{"vote"}. How to 
    summarize the predictions of the multiple models in the fitting result
    of \code{\link{generic.bagging}}. \code{"mean"} will produce the average of
    the predictions, while \code{"vote"} will select the prediction with the
    most votes.
  }
  
  \item{\dots}{
    Extra parameters. Not implemented yet.
  }
}

\value{
  A \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  to compute the prediction.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{generic.bagging}} generates the models of boostrap
  aggregating.

  \code{\link{predict.lm.madlib}} and
  \code{\link{predict.logregr.madlib}} produce predictions for linear
  and logistic models.
}
\examples{
\dontrun{
## set up the database connection
db.connect(port = 14526, dbname = "madlib")

y <- db.data.frame("abalone")

fit <- generic.bagging(function(data) {
                           madlib.lm(rings ~ . - id - sex, data = data)
                       }, data = y, nbags = 25, fraction = 0.7)

pred <- predict(fit, newdata = y) # make prediction

lookat(mean((y$rings - pred)^2)) # mean squared error
}
}

\keyword{ methods }
\keyword{ math }
\keyword{ stats }
