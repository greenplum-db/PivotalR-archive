\name{margins}
\alias{margins}
\alias{margins.lm.madlib}
\alias{margins.lm.madlib.grps}
\alias{margins.logregr.madlib}
\alias{margins.logregr.madlib.grps}
\alias{print.margins}
\alias{Vars}

\title{
Compute the marginal effects of regression models
}

\description{
  \code{margins} calculates the marginal effects of the variables given the result of regressions (\code{\link{madlib.lm}}, \code{\link{madlib.glm}} etc). \code{Vars} lists all the variables used in the regression model. \code{Vars} is only used in \code{margins}.
}

\usage{
\method{margins}{lm.madlib}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{lm.madlib.grps}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{logregr.madlib}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{margins}{logregr.madlib.grps}(model, dydx = ~Vars(model), newdata = model$data, at.mean = FALSE, factor.continuous = FALSE, na.action = NULL, ...)

\method{print}{margins}(x, digits = max(3L, getOption("digits") - 3L), ...)

Vars(model)
}

\arguments{
  \item{model}{
    The result of \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, which represents a regression model for the training data.
}
  \item{dydx}{
    A formula, and the default is \code{~ Vars(model)}, which tells the function to compute the marginal effects for all the variables that appear in the model. \code{~ .} will compute the marginal effects of all variables in \code{newdata}.
}
  \item{newdata}{
    A \code{\linkS4class{db.obj}} object, which represents the data in the database. The default is the data used to train the regression model, but the user can freely use other data sets.
}
  \item{at.mean}{
    A logical, the default is \code{FALSE}. Whether to compute the marginal effects at the mean values of the variables.
}
  \item{factor.continuous}{
    A logical, the default is \code{FALSE}. Whether to compute the marginal effects of factors by treating them as continuous variables. See "details" for more explanation.
}
  \item{na.action}{
        A string which indicates what should happen when the data
        contain \code{NA}s. Possible
        values include \code{\link{na.omit}}, \code{"na.exclude"},              \code{"na.fail"}
  and \code{NULL}. Right now, \code{\link{na.omit,db.obj-method}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered on the MADlib side. User defined \code{na.action} function is allowed, and see \code{\link{na.omit,db.obj-method}} for the preferred function interface.
}
  \item{\dots}{
    Other arguments, not implemented.
  }

  \item{x}{
    The result of \code{margins} function, which is of the class "margins".
  }

  \item{digits}{
            A non-null value for `digits' specifies the minimum number of
          significant digits to be printed in values.  The default,
          `NULL', uses `getOption("digits")'.  (For the interpretation
          for complex numbers see \code{signif}.)  Non-integer values will
          be rounded down, and only values greater than or equal to 1
          and no greater than 22 are accepted.
  }
}

\details{
  For a continuous variable, its marginal effects is just the first derivative of the response function with respect to the variable. For a categorical variable, it is usually more meaningful to compute the finite difference of the response function for the variable being 1 and 0. The finite difference marginal effect represents the 
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