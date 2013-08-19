\name{generic.cv}
\alias{generic.cv}

\title{
  Generic cross-validation for supervised learning algorithms
}

\description{
  This function runs cross-validation for a given supervised learning
  model, which is specified by the training function, prediction
  function, and metric function. The user might need to write wrappers
  for the functions so that they satisfy the format requirements
  desceribed in the following.
}

\usage{
generic.cv(train, predict, metric, data, params = NULL, k = 10)
}

\arguments{
  \item{train}{
    A training function. It must have at least one argument named
    \code{data}. Given the data, it produces the model. It can also have
  other parameters that specifies the model, and these parameters must
  appear in the list \code{params}.
  }
  
  \item{predict}{
    A prediction function. It must have only two arguments named
    \code{object} and \code{newdata}, which are the fitted model and
    the new data input for prediction.
  }
  
  \item{metric}{
    A metric function. It must have only two arguments named
    \code{predicted} and \code{data}, where \code{predicted} is the
    redictions. 
  }
  
  \item{data}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the
    database, used for cross-validation.
  }
  
  \item{params}{
    A list, default is \code{NULL}. The values of each parameters used
    by the training function. An array of values for each parameter is
    an element 
    in the list. The value arrays for different parameters do not have
    to be the same length. The arrays of shorter lengths are circularly
    expanded to the length of the longest element. 
  }
  
  \item{k}{
    An integer, default is 10. The cross-validation fold number.
  }
}

\value{
  A \code{data.frame}, which contains two elements: \code{err} and
  \code{err.std}, which are the errors and 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{generic.bagging}} does the boostrap aggregate computation.
}

\examples{
\dontrun{
## set up the database connection
db.connect(port = 14526, dbname = "madlib")

y <- db.data.frame("abalone")

err <- generic.cv(function(data) {
                      madlib.lm(rings ~ . - id - sex, data = data)
                  },
                  predict,
                  function(predicted, data) {
                      lookat(mean((data$rings - predicted)^2))
                  }, data = y)
}
}

\keyword{ utility }
\keyword{ math }
