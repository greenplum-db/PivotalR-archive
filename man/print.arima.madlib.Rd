\name{print.arima.madlib}
\alias{print.arima.css.madlib}
\alias{show.arima.css.madlib}

\title{Display results of ARIMA fitting of \code{madlib.arima}}

\description{
    This function displays the results of \code{\link{madlib.arima}} in
    a pretty format. 
}

\usage{
\method{print}{arima.css.madlib}(x, digits = max(3L, getOption("digits")
- 3L), ...)

\method{show}{arima.css.madlib}(object)
}

\arguments{
    \item{x, object}{
        The ARIMA fitting result object of \code{\link{madlib.arima}}
    }

    \item{digits}{
        A non-null value for `digits' specifies the minimum number of
          significant digits to be printed in values.  The default,
          `NULL', uses `getOption("digits")'.  (For the interpretation
          for complex numbers see \code{signif}.)  Non-integer values will
          be rounded down, and only values greater than or equal to 1
          and no greater than 22 are accepted.
    }

    \item{\dots}{
      Further arguments passed to or from other methods.
      This is currently not implemented.
    }
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{madlib.arima}} Wrapper for MADlib ARIMA model fitting
}

\examples{
\dontrun{
## Please see the examples in madlib.arima doc
}
}

\keyword{IO}
\keyword{madlib}
\keyword{utility}