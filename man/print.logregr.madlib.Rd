\name{print.logregr.madlib}
\alias{print.logregr.madlib}
\alias{show.logregr.madlib}

\title{Display results of logistic regression}

\description{
    This function displays the results of logistic regression in a pretty format.
}

\usage{
\method{print}{logregr.madlib}(x, digits = max(3L, getOption("digits") - 3L), ...)

\method{show}{logregr.madlib}(object)
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
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{madlib.glm}} Wrapper for MADlib linear and logistic regression
}

\examples{
    \dontrun{
## Suppose that a valid connection with ID 1 exists
## create a table from the example data.frame
source_data <- db.data.frame("madlibtestdata.log_breast_cancer_wisconsin")
preview(source_data, 10)

## logistic regression
fit <- madlib.logregr(y ~ x - 1 , data = source_data)
print(fit)
    }
}

\keyword{IO}
\keyword{print}
