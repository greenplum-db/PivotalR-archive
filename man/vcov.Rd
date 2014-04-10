\name{vcov}

\alias{vcov.logregr.madlib}
\alias{vcov.lm.madlib}
\alias{vcov.logregr.madlib.grps}
\alias{vcov.lm.madlib.grps}

\title{vcov methods for Madlib regression objects}

\description{
    Functions to extract the variance-cocariance matrix for regression models fit in Madlib.
}

\usage{
    \method{vcov}{lm.madlib.grps}(object, na.action = NULL, ...)
    \method{vcov}{lm.madlib}(object, na.action = NULL, ...)
    \method{vcov}{logregr.madlib.grps}(object, na.action = NULL, ...)
    \method{vcov}{logregr.madlib}(object, na.action = NULL, ...)
}

\arguments{
    \item{object}{
        The regression model object, of class \code{lm.madlib}, \code{lm.madlib.grps}, \code{logregr.madlib}, \code{logregr.madlib.grps}.
    }

    \item{na.action}{
        A function, default is \code{NULL}. Possible choice is \code{\link{na.omit,db.obj-method}}.
    }

    \item{...}{
        Other arguments, not used.
    }
}

\value{
  For \code{lm.madlib} and \code{logregr.madlib} objects, this function   returns the variance-covariance matrix of the main parameters.

For \code{lm.madlib.grps} and \code{logregr.madlib.grps} objects, which are a list of models for multiple groups of data, returns a list, each of which is the variance-cocariance matrix for the model of each group of data.
}

\author{
  Author: Hong Ooi, Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.lm}}, \code{\link{madlib.glm}} for MADlib regression wrappers
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family =
"binomial")

vcov(fit)

vcov(fit[[1]])

db.disconnect(cid, verbose = FALSE)
}
}
