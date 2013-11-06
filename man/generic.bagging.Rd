\name{generic.bagging}
\alias{generic.bagging}

\title{
  This function runs boostrap aggregating for a given training function.
}

\description{
  A generic function to do boostrap aggregating for a given machine
  learning model. The user might need to write a wrapper
  for the training function so that they could satisfy the format
  requirements 
  desceribed in the following.
}

\usage{
generic.bagging(train, data, nbags = 10, fraction = 1)
}

\arguments{
  \item{train}{
    A training function. It must have only one argument named
  \code{data}. Given the data, it produces the model.
  }
  
  \item{data}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the
    database. 
  }
  
  \item{nbags}{
    An integer, default is 10. The number of bagging sampling.
  }
  
  \item{fraction}{
    A double, default is 1. The fraction of data in each bagging
    sample. 
  }
}

\value{
  A \code{bagging.model} object, which is actually a list of fitted
  models. 
}

\references{
  [1] Wiki: bagging \url{http://en.wikipedia.org/wiki/Bootstrap_aggregating}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{predict.bagging.model}} makes predictions using the result
  of this function.
  
  \code{\link{generic.cv}} for cross-validation

  \code{\link{sample,db.obj-method}} samples data from a table
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

\keyword{math}
\keyword{stats}