\name{as.factor}
\alias{as.factor}
\alias{as.factor,db.obj-method}

\title{ Convert one column of a \code{db.obj} object into a categorial variable }

\description{
  Convert one column of a \code{db.obj} object into a categorial
  variable. When \code{\link{madlib.lm}} or \code{\link{madlib.glm}} are
  applied onto a \code{db.obj} with categorial columns, dummy columns
  will be created and fitted.
}

\usage{
\S4method{as.factor}{db.obj}(x)
}

\arguments{
  \item{x}{
    A \code{db.obj} object. It must have only one column.
  }
}

\value{
  A \code{db.Rquery} object. It has only one column which is categorial.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{madlib.lm}} and \code{\link{madlib.glm}} can fit
  categorial variables

  When \code{\link{as.db.data.frame}} creates a table/view, it can create dummy variables for a
  categorial variable.
}

\examples{
\dontrun{

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

## set sex to be a categorial variable
x$sex <- as.factor(x$sex)

fit <- madlib.lm(rings ~ . - id, data = x)
}
}

\keyword{methods}
\keyword{ factor }
