\name{summary.logregr.madlib}
\alias{summary.logregr.madlib}

\title{Summary information for Logistic Regression output}
\description{
    The function prints information about the logistic regression object along with
    the value for each returned element.
}

\usage{
    \method{summary}{logregr.madlib}(object, ...)
}

\arguments{
    \item{object}{Linear regression object}

    \item{...}{
      Further arguments passed to or from other methods.
      This is currently not implemented.}
}

\value{
    The function returns the \code{logregr.madlib} object passed to the function.

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}


\seealso{
    \code{\link{madlib.lm}} Wrapper for MADlib linear regression
}


\examples{
    \dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame
source_data <- db.data.frame("madlibtestdata.log_breast_cancer_wisconsin")
preview(source_data, 10)

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.logregr(y ~ x , data = source_data)
fit

## use I(.) for expressions
fit <- madlib.logregr(y ~ x + I(x^2), data = source_data)
summary(fit)
       }
}

\keyword{madlib}
\keyword{summary}