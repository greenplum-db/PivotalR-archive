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
    \method{vcov}{lm.madlib.grps}(object, ...)
    \method{vcov}{lm.madlib}(object, ...)
    \method{vcov}{logregr.madlib.grps}(object, ...)
    \method{vcov}{logregr.madlib}(object, ...)
}

\arguments{
    \item{object}{
        The regression model object, of class \code{lm.madlib}, \code{lm.madlib.grps}, \code{logregr.madlib}, \code{logregr.madlib.grps}.
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

\examples{ \dontrun{
## create a table
x <- as.db.data.frame(abalone, "abalone")

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family =
"binomial")

vcov(fit)

vcov(fit[[1]])
} }


