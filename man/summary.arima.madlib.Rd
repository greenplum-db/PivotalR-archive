\name{summary.arima.madlib}
\alias{summary.arima.css.madlib}

\title{Summary information for MADlib's ARIMA model}
\description{
    The function prints the result of \code{\link{madlib.arima}} in a
    pretty format
}

\usage{
\method{summary}{arima.css.madlib}(object, ...)
}

\arguments{
  \item{object}{
    The ARIMA fitting result object of \code{\link{madlib.arima}}
  }

  \item{\dots}{
    Further arguments passed to or from other methods.
    This is currently not implemented.
  }
}

\value{
    The function returns the object passed to the function 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
  \code{\link{madlib.arima}} Wrapper for MADlib ARIMA model fitting

  \code{\link{print.arima.css.madlib}} print the ARIMA result 
}

\examples{
\dontrun{
## Please see the examples in madlib.arima doc
}
}

\keyword{madlib}
\keyword{stats}
\keyword{math}
