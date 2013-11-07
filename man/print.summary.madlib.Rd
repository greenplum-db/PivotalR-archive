\name{print.summary.madlib}
\alias{print.summary.madlib}
\alias{show.summary.madlib}

\title{Display the results from summary function in a pretty format}

\description{
    This function prints the results from \code{\link{madlib.summary}} in a human-readable format.
}

\usage{
\method{print}{summary.madlib}(x, digits = max(3L,getOption("digits") - 3L),...)

\method{show}{summary.madlib}(object)
}


\arguments{
    \item{x, object}{
        The summary result object to be printed.
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
    \code{\link{madlib.summary}} Wrapper for MADlib linear and logistic regression
}

\examples{
\dontrun{
## Suppose that a valid connection with ID 1 exists
## create a table from the example data.frame

x  <- db.data.frame("madlibtestdata.lin_ornstein")

preview(x, 10)

# madlib.summary
summary_result  <- madlib.summary(x)
print(summary_result)
show(summary_result)
    }
}

\keyword{IO}
\keyword{madlib}
\keyword{utilities}

