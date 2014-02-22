\name{coef}

\alias{coef.logregr.madlib}
\alias{coef.lm.madlib}
\alias{coef.logregr.madlib.grps}
\alias{coef.lm.madlib.grps}

\title{Extract model coefficients for Madlib regression objects}

\description{
    Functions to extract the coefficients for regression models fit in Madlib.
}

\usage{
    \method{coef}{lm.madlib}(object, ...)
    \method{coef}{lm.madlib.grps}(object, ...)
    \method{coef}{logregr.madlib}(object, ...)
    \method{coef}{logregr.madlib.grps}(object, ...)
}

\arguments{
    \item{object}{
        The regression model object, fit using \code{\link{madlib.lm}} or \code{\link{madlib.glm}}.
    }
    \item{...}{
        Other arguments, not used.
    }
}

\details{
    Extract the fitted coefficients for a linear or logistic regression model, or a grouped list of such models.
}

\value{ For ungrouped regressions, a named numeric vector giving the fitted coefficients.

    For grouped regressions, a list giving the coefficients for each of the component
  models.
}

\author{ Author: Hong Ooi, Pivotal Inc. \email{hooi@gopivotal.com}

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{coef}}.
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

coef(fit)

coef(fit[[1]])

db.disconnect(cid, verbose = FALSE)
} }

\keyword{utility}