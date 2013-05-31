\name{summary.lm.madlib}
\alias{summary.lm.madlib}

\title{Summary information about Linear Regression output}
\description{
    The function prints information about the linear regression object with
    the value for each returned element.
}

\usage{
\method{summary}{lm.madlib}(object, ...)
}


\arguments{
    \item{object}{Linear regression object}

    \item{...}{
      Further arguments passed to or from other methods.
      This is currently not implemented.}
}

\value{
    The function returns the \code{lm.madlib} object passed to the function
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
x <- db.data.frame("madlibtestdata.lin_ornstein")
preview(x, 10)

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.lm(interlocks ~ . | nation, data = x, heteroskedasticity = T)
fit

## use I(.) for expressions
fit <- madlib.lm(interlocks ~ assets + sector + nation + I(assets^2),
data = x, heteroskedasticity = T)
summary(fit)
}
}

\keyword{madlib}
\keyword{summary}