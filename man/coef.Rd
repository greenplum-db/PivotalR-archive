\name{coef}

\alias{coef.logregr.madlib}

\alias{coef.logregr.madlib.grps}


\title{Extract model coefficients for Madlib regression objects}

\description{
    Functions to extract the coefficients for regression models fit in Madlib.
}

\usage{

    \method{coef}{logregr.madlib}(object, ...)
    \method{coef}{logregr.madlib.grps}(object, ...)
}

\arguments{

    \item{...}{
        Other arguments, not used.
    }
}

\details{
    Extract the fitted coefficients for a linear or logistic regression model, or a grouped list of such models.
}

\value{
}

\author{ Author: Hong Ooi, Pivotal Inc. \email{hooi@gopivotal.com}

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{coef}}.
}

\examples{ \dontrun{

## create a table
x <- as.db.data.frame(abalone, "abalone")

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family =
"binomial")

coef(fit)

coef(fit[[1]])
} }

\keyword{utility}