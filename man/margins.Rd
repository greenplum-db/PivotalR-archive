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
  \code{margins} calculates the marginal effects of the variables given the result of regressions (\code{\link{madlib.lm}}, \code{\link{madlib.glm}} etc). \code{Vars} lists all the variables used in the regression model. \code{Terms} lists the specified terms in the original model. \code{Vars} and \code{Terms} are only used in \code{margins}'s \code{dydx} option.
}

\usage{
\method{margins}{lm.madlib}(model, dydx = ~Vars(model), newdata =
model$data, at.mean = FALSE, factor.continuous = FALSE, na.action =
NULL, ...)

\method{margins}{lm.madlib.grps}(model, dydx = ~Vars(model), newdata =
lapply(model, function(x) x$data), at.mean = FALSE, factor.continuous =
FALSE, na.action = NULL, ...)

\method{margins}{logregr.madlib}(model, dydx = ~Vars(model), newdata =
model$data, at.mean = FALSE, factor.continuous = FALSE, na.action =
NULL, ...)

\method{margins}{logregr.madlib.grps}(model, dydx = ~Vars(model),
newdata = lapply(model, function(x) x$data), at.mean = FALSE,
factor.continuous = FALSE, na.action = NULL, ...)

\method{print}{margins}(x, digits = max(3L, getOption("digits") - 3L),
...)

Vars(model)

Terms(term = NULL)
}

\arguments{
  \item{model}{
    The result of \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, which represents a regression model for the training data.
}
  \item{dydx}{
    A formula, and the default is \code{~ Vars(model)}, which tells the function to compute the marginal effects for all the variables that appear in the model. \code{~ .} will compute the marginal effects of all variables in \code{newdata}. Use the normal formula to specify which variables' marginal effects are to be computed.
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
  and \code{NULL}. Right now, \code{\link{na.omit,db.obj-method}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered out and omitted on the MADlib side. User defined \code{na.action} function is allowed, and see \code{\link{na.omit,db.obj-method}} for the preferred function interface.
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

  \item{term}{
      A vector of integers, the default is \code{NULL}. When \code{term=i}, compute the marginal effects of the i-th term. Even if this term contains multiple variables, we treat it as a variable independent of all others. When \code{term=NULL}, the marginal effects of all terms are calculated. In the final result, margianl effect results for \code{".term.1"}, \code{".term.2"} etc will be shown. By comparing with \code{names(model$coef)}, one can easily figure out which term corresponds to which expression. \code{(Intercept)} term's marginal effect cannot be computed using this (One can create an extra column that equals 1 and use it as a variable without using intercept by add -1 into the fitting formula).
  }
}

\details{
  For a continuous variable, its marginal effects is just the first derivative of the response function with respect to the variable. For a categorical variable, it is usually more meaningful to compute the finite difference of the response function for the variable being 1 and 0. The finite difference marginal effect measures how much more the response function would be compared with the reference category. The reference category for a categorical variable can be changed by \code{\link{relevel}}.
}

\value{
  \code{margins} function returns a \code{margins} object, which is a   \code{data.frame}. It contains the following item:
\item{Estimate}{
  The marginal effect values for all variable that have been specified in \code{dydx}.
}

\item{Std. Error}{
  The standard errors for the marginal effects.
}

\item{t value, z value}{
  The t statistics (for linear regression) or z statistics (for logistic regression).
}

\item{Pr(>|t|), Pr(>|z|)}{
  The corresponding p values.
}

\code{Vars} returns a vector of strings, which are the variable names that have been used in the regression model.
}

\references{
[1] Stata 13 help for margins, \url{http://www.stata.com/help.cgi?margins}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{relevel}} changes the reference category.

  \code{\link{madlib.lm}}, \code{\link{madlib.glm}} compute linear and logistic regressions.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname)

## create a data table in database and the R wrapper
delete("abalone", conn.id = cid)
dat <- as.db.data.frame(abalone, "abalone", conn.id = cid)

fit <- madlib.lm(rings ~ length + diameter*sex, data = dat)
margins(fit)
margins(fit, at.mean = TRUE)
margins(fit, factor.continuous = TRUE)
margins(fit, dydx = ~ Vars(model) + Terms())

fit <- madlib.glm(rings < 10 ~ length + diameter*sex, data = dat, family = "logistic")
margins(fit, ~ length + sex)
margins(fit, ~ length + sex.M, at.mean = TRUE)
margins(fit, ~ length + sex.I, factor.continuous = TRUE)
margins(fit, ~ Vars(model) + Terms())

## create a data table that has two columns
## one of them is an array column
dat1 <- cbind(db.array(dat[,-c(1,2,10)]), dat[,10])
names(dat1) <- c("x", "y")
delete("abalone_array", conn.id = cid)
dat1 <- as.db.data.frame(dat1, "abalone_array")

fit <- madlib.glm(y < 10 ~ x[-1], data = dat1, family = "logistic")
margins(fit, ~ x[2:5])

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{stats}
\keyword{math}
