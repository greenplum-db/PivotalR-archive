\name{print.logregr.madlib}
\alias{print.logregr.madlib}
\alias{show.logregr.madlib}
\alias{print.logregr.madlib.grps}
\alias{show.logregr.madlib.grps}

\title{Display results of logistic regression}

\description{
    This function displays the results of logistic regression in a pretty format.
}

\usage{
\method{print}{logregr.madlib}(x, digits = max(3L, getOption("digits") -
3L), ...)

\method{print}{logregr.madlib.grps}(x, digits = max(3L,
getOption("digits") - 3L), ...)

\method{show}{logregr.madlib}(object)

\method{show}{logregr.madlib.grps}(object)
}

\arguments{
    \item{x, object}{
        The logistic regression result object to be printed.
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

\value{
    No value is returned
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{madlib.glm}} Wrapper for MADlib linear and logistic regression
}

\examples{
\dontrun{
## see the examples in madlib.glm
}
}

\keyword{IO}
\keyword{madlib}
\keyword{utility}
