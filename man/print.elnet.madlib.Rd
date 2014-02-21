\name{print.elnet.madlib}
\alias{print.elnet.madlib}
\alias{show.elnet.madlib}

\title{
  Display the results from madlib.elnet function in a pretty format
}

\description{
    This function prints the results from \code{\link{madlib.elnet}} in a human-readable format.
}

\usage{
\method{print}{elnet.madlib}(x, digits = max(3L, getOption("digits") - 3L), ...)

\method{show}{elnet.madlib}(object)
}

\arguments{
  \item{x, object}{
    The \code{elnet.madlib} object to be printed.
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
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.elnet}} Wrapper for MADlib elastic net regularization.
}
\examples{
## see the examples in madlib.elnet
}

\keyword{IO}
\keyword{madlib}
\keyword{utilities}