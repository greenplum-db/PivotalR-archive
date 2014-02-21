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
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{madlib.lm}} Wrapper for MADlib linear regression
}

\examples{
## see the examples in madlib.lm
}

\keyword{madlib}
\keyword{stats}
\keyword{math}