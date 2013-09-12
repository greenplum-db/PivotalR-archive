\name{summary.lm.madlib}
\alias{summary.lm.madlib}
\alias{summary.lm.madlib.grps}

\title{Summary information for Linear Regression output}
\description{
    The function prints the value of each element in the Linear
    Regression output object.
}

\usage{
\method{summary}{lm.madlib}(object, ...)

\method{summary}{lm.madlib.grps}(object, ...)
}


\arguments{
    \item{object}{Linear regression object}

    \item{\dots}{
      Further arguments passed to or from other methods.
      This is currently not implemented.}
}

\value{
    The function returns the \code{lm.madlib} or \code{lm.madlib.grps}
    object passed to the function 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
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
summary(fit[[1]]) # one model for a group
summary(fit) # models for all groups

## use I(.) for expressions
fit <- madlib.lm(interlocks ~ assets + sector + nation + I(assets^2),
data = x, heteroskedasticity = T)
summary(fit)
}
}

\keyword{madlib}
\keyword{stats}
\keyword{math}