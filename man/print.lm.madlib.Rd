\name{print.lm.madlib}
\alias{print.lm.madlib}
\alias{print.lm.madlib.grps}
\alias{show.lm.madlib}
\alias{show.lm.madlib.grps}

\title{Display results of linear regression}

\description{
    This function displays the results of linear regression in a pretty format.
}

\usage{
\method{print}{lm.madlib}(x, digits = max(3L, getOption("digits") - 3L),
...)

\method{print}{lm.madlib.grps}(x, digits = max(3L, getOption("digits") -
3L), ...) 

\method{show}{lm.madlib}(object)

\method{show}{lm.madlib.grps}(object)
}

\arguments{
    \item{x, object}{
        The linear regression result object to be printed.
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

## display the model for one group 
print(fit[[1]])
show(fit[[1]])

## display the models for all groups
print(fit)
show(fit)
    }
}

\keyword{IO}
\keyword{print}
\keyword{utilities}