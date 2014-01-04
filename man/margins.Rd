\name{margins}
\alias{margins}
\alias{margins.lm.madlib}
\alias{margins.lm.madlib.grps}
\alias{margins.logregr.madlib}
\alias{margins.logregr.madlib.grps}
\alias{print.margins}
\alias{Vars}
\alias{Terms}

\title{
Compute the marginal effects of regression models
}

\description{
  
}

\usage{
\method{margins}{lm.madlib}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{lm.madlib.grps}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{logregr.madlib}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{logregr.madlib.grps}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{print}{margins}(x, digits = max(3L, getOption("digits") - 3L), ...)

Vars(model)

Terms(term = NULL)
}

\arguments{
  \item{model}{

}
  \item{dydx}{

}
  \item{newdata}{

}
  \item{at.mean}{

}
  \item{factor.continuous}{

}
  \item{na.action}{

}
  \item{\dots}{

  }

  \item{x}{

  }

  \item{digits}{

  }

  \item{term}{

  }
}

\details{

}

\value{

}

\references{

}

\author{

}

\note{

}

\seealso{

}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, 
    factor.continuous = FALSE, na.action = NULL, ...) 
UseMethod("margins", model)
}

\keyword{stats}
\keyword{math}