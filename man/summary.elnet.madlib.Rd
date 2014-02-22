\name{summary.elnet.madlib}
\alias{summary.elnet.madlib}

\title{
  Summary information for Elastic net regularization output
}
\description{
  The function prints the value of each element in the output object of \code{\link{madlib.elnet}}.
}
\usage{
\method{summary}{elnet.madlib}(object, ...)
}

\arguments{
  \item{object}{
    A \code{elnet.madlib} object produced by \code{\link{madlib.elnet}}.
}
  \item{\dots}{
    Further arguments passed to or from other methods.
    This is currently not implemented.
}
}

\value{
  The function returns the \code{elnet.madlib} object in the argument.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.elnet}} Wrapper for MADlib elastic net regularization.
}
\examples{
## see the examples in madlib.elnet
}

\keyword{madlib}
\keyword{stats}
\keyword{math}
