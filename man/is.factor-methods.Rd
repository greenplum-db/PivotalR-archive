\name{is.factor-methods}

\alias{is.factor,db.obj-method}

\title{ Detect whether a \code{\linkS4class{db.obj}} object is a
  categorial object }

\description{
  This function detects whether a \code{\linkS4class{db.obj}} object is a
  categorial object. 
}

\usage{
\S4method{is.factor}{db.obj}(x)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object. 
  }
}

\value{
  A logical value. When all columns of \code{\linkS4class{db.obj}} are
  categorial variables, this function returns \code{TRUE}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{as.factor,db.obj-method}} converts a column
  \code{\linkS4class{db.obj}} of into categorial variables.
}

\examples{
\dontrun{
## Suppose that a valid connection with ID 1 is already created by db.connect

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone") # use default connection 1

## set sex to be a categorial variable
x$sex <- as.factor(x$sex)

is.factor(x$sex)

is.factor(x)
}
}

\keyword{methods}
\keyword{ factor }
