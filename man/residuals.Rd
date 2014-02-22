\name{residuals}

\alias{residuals.logregr.madlib}
\alias{residuals.logregr.madlib.grps}
\alias{residuals.lm.madlib}
\alias{residuals.lm.madlib.grps}

\title{residuals methods for Madlib regression objects}

\description{
    Functions to extract the residuals for regression models fit in Madlib.
}

\usage{
    \method{residuals}{lm.madlib}(object, ...)
    \method{residuals}{lm.madlib.grps}(object, ...)
    \method{residuals}{logregr.madlib}(object, ...)
    \method{residuals}{logregr.madlib.grps}(object, ...)
}

\arguments{
    \item{object}{
        The regression model object, of class \code{lm.madlib}, \code{lm.madlib.grps} or \code{logregr.madlib}, \code{logregr.madlib.grps} obtained using \code{\link{madlib.lm}} or \code{\link{madlib.glm}} respectively.
    }
    \item{...}{
        Other arguments, not used.
    }
}

\details{
    See the documentation for \code{\link{residuals}}
}

\value{
  For ungrouped regressions, \code{residuals} returns an object of class \code{\linkS4class{db.Rquery}}

    For grouped regressions, \code{residuals} returns a list of \code{\linkS4class{db.Rquery}} objects giving the output of these methods for each of the component models. Similarly, \code{AIC} for a grouped regression returns a vector of the AICs for each of the component models.
}

\author{
  Author: Predictive Analytics Team, Pivotal Inc.

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

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family =
"binomial")

residuals(fit)

db.disconnect(cid)
}
}
