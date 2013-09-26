\name{AIC}

\alias{AIC.logregr.madlib}
\alias{extractAIC.logregr.madlib}
\alias{logLik.logregr.madlib}
\alias{AIC.lm.madlib}
\alias{extractAIC.lm.madlib}
\alias{logLik.lm.madlib}

\title{AIC methods for Madlib regression objects}

\description{
    Functions to extract the AIC and log-likelihood for regression models fit in Madlib. Currently only implemented for logistic regression.
}

\usage{
    \method{extractAIC}{logregr.madlib}(fit, scale=0, k=2, ...)
    \method{logLik}{logregr.madlib}(object, ...)
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

\value{
    The \code{logLik} function returns an object of class \code{logLik}. The \code{extractAIC} function returns a length-2 numeric vector giving the edf and AIC respectively.
}

\seealso{
    \code{\link{AIC}}, \code{\link{extractAIC}}, \code{\link{logLik}}.
}


