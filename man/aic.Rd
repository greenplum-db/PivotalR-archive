\name{AIC}

\alias{AIC.logregr.madlib}
\alias{extractAIC.logregr.madlib}
\alias{logLik.logregr.madlib}
\alias{AIC.lm.madlib}
\alias{extractAIC.lm.madlib}
\alias{logLik.lm.madlib}
\alias{AIC.logregr.madlib.grps}
\alias{extractAIC.logregr.madlib.grps}
\alias{logLik.logregr.madlib.grps}
\alias{AIC.lm.madlib.grps}
\alias{extractAIC.lm.madlib.grps}
\alias{logLik.lm.madlib.grps}

\title{AIC methods for Madlib regression objects}

\description{
    Functions to extract the AIC and log-likelihood for regression models fit in Madlib.
}

\usage{
    \method{extractAIC}{lm.madlib}(fit, scale=0, k=2, ...)
    \method{extractAIC}{lm.madlib.grps}(fit, scale=0, k=2, ...)
    \method{logLik}{lm.madlib}(object, ...)
    \method{logLik}{lm.madlib.grps}(object, ...)
    \method{AIC}{lm.madlib.grps}(object, ..., k=2)

    \method{extractAIC}{logregr.madlib}(fit, scale=0, k=2, ...)
    \method{extractAIC}{logregr.madlib.grps}(fit, scale=0, k=2, ...)
    \method{logLik}{logregr.madlib}(object, ...)
    \method{logLik}{logregr.madlib.grps}(object, ...)
    \method{AIC}{logregr.madlib.grps}(object, ..., k=2)
}

\arguments{
    \item{fit, object}{
        The regression model object, of class \code{lm.madlib} or \code{logregr.madlib}, fit using \code{\link{madlib.lm}} or \code{\link{madlib.glm}} respectively.
    }
    \item{scale}{
        The scale parameter for the model. Currently unused.
    }
    \item{k}{
        Numeric, specifying the equivalent degrees of freedom part in the AIC formula.
    }
    \item{...}{
        Other arguments, not used.
    }
}

\details{
    See the documentation for \code{\link{AIC}} and \code{\link{extractAIC}}.
}

\value{ For ungrouped regressions, \code{logLik} returns an object of
    class \code{logLik}, and \code{extractAIC} returns a length-2
    numeric vector giving the edf and AIC.

    For grouped regressions, \code{logLik} and \code{extractAIC} return
  a list giving the output of these methods for each of the component
  models. Similarly, \code{AIC} for a grouped regression returns a
  vector of the AICs for each of the component models.  }

\author{
  Author: Hong Ooi, Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{AIC}}, \code{\link{extractAIC}}, \code{\link{logLik}}.
}

\examples{ \dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family =
"binomial")

AIC(fit)

AIC(fit[[1]])

db.disconnect(cid, verbose = FALSE)
}
}
