\name{madlib.glm}
\alias{madlib.glm}
\alias{coef.logregr.madlib}
\alias{coef.logregr.madlib.grps}
\alias{summary.logregr.madlib}
\alias{summary.logregr.madlib.grps}
\alias{print.logregr.madlib}
\alias{print.logregr.madlib.grps}
\alias{show.logregr.madlib}
\alias{show.logregr.madlib.grps}
\alias{vcov.logregr.madlib}
\alias{vcov.logregr.madlib.grps}
\alias{groups.logregr.madlib}
\alias{groups.logregr.madlib.grps}
\alias{residuals.logregr.madlib}
\alias{residuals.logregr.madlib.grps}
\alias{AIC.logregr.madlib}
\alias{extractAIC.logregr.madlib}
\alias{logLik.logregr.madlib}
\alias{AIC.logregr.madlib.grps}
\alias{extractAIC.logregr.madlib.grps}
\alias{logLik.logregr.madlib.grps}
\alias{predict.logregr.madlib}
\alias{predict.logregr.madlib.grps}

\title{Linear regression, Logistic Regression, and Multinomial Logistic Regression. Other useful function to extract fitted coefficients, measure the variance-covariance matrix, make predictions etc.}

\description{
  The wrapper function for MADlib linear regression and logistic regression.
  Heteroskedasticity test is implemented for linear regression . One or multiple
  columns of data can be used to separate the data set into multiple groups
  according to the values of the grouping columns. The requested regression
  method is applied onto each group, which has fixed values of the grouping
  columns. Multinomial logistic regression is not implemented
  yet. Categorical variables are supported. The
  computation is parallelized by MADlib if the connected database is
  Greenplum database. The regression computation can also be done on a
  column that is an array in the data table.
}

\usage{
madlib.glm(formula, data, family = c("gaussian", "linear", "binomial",
"logistic"), na.action = NULL, control = list(), ...)

\method{coef}{logregr.madlib}(object, ...)

\method{coef}{logregr.madlib.grps}(object, ...)

\method{summary}{logregr.madlib}(object, ...)

\method{summary}{logregr.madlib.grps}(object, ...)

\method{print}{logregr.madlib}(x, digits = max(3L, getOption("digits") - 3L),
...)

\method{print}{logregr.madlib.grps}(x, digits = max(3L, getOption("digits") -
3L), ...)

\method{show}{logregr.madlib}(object)

\method{show}{logregr.madlib.grps}(object)

\method{vcov}{logregr.madlib.grps}(object, ...)

\method{vcov}{logregr.madlib}(object, ...)

\method{groups}{logregr.madlib}(x)

\method{groups}{logregr.madlib.grps}(x)

\method{residuals}{logregr.madlib}(object, ...)

\method{residuals}{logregr.madlib.grps}(object, ...)

\method{extractAIC}{logregr.madlib}(fit, scale=0, k=2, ...)

\method{extractAIC}{logregr.madlib.grps}(fit, scale=0, k=2, ...)

\method{logLik}{logregr.madlib}(object, ...)

\method{logLik}{logregr.madlib.grps}(object, ...)

\method{AIC}{logregr.madlib.grps}(object, ..., k=2)

\method{predict}{logregr.madlib}(object, newdata, ...)

\method{predict}{logregr.madlib.grps}(object, newdata, ...)
}

\arguments{
    \item{x, object, fit}{
    The regression model object, fit using \code{\link{madlib.logregr}}.
  }

      \item{scale}{
        The scale parameter for the model. Currently unused.
      }

    \item{k}{
        Numeric, specifying the equivalent degrees of freedom part in the AIC formula.
    }

    \item{digits}{
        A non-null value for `digits' specifies the minimum number of
          significant digits to be printed in values.  The default,
          `NULL', uses `getOption("digits")'.  (For the interpretation
          for complex numbers see \code{signif}.)  Non-integer values will
          be rounded down, and only values greater than or equal to 1
          and no greater than 22 are accepted.
        }

        \item{newdata}{
          A \code{db.obj} object, which contains the information about the real data in the database.
  }

  \item{type}{
    A string, default is \code{"response"}, which produces the \code{TRUE} or \code{FALSE} prediction. If it is \code{"prob"}, this function computes the probabilities for \code{TRUE} cases.
  }

    \item{formula}{
        An object of class \code{\link{formula}} (or one that can be coerced to
        that class): a symbolic description of the model to be
        fitted. The details of model specification are given under `Details'.
    }

    \item{data}{
        An object of \code{db.obj} class. Currently, this parameter is
        mandatory. If it is an object of class \code{db.Rquery} or
        \code{db.view}, a temporary table will be created, and further
        computation will be done on the temporary table. After the
        computation, the temporary will be dropped from the corresponding
        database.
    }

    \item{family}{
        A string which indicates which form of regression to apply. Default value
        is ``gaussian''.
        The accepted values are:
           ``gaussian'' or ``linear'': Linear regression
           ``binomial'' or ``logistic'': Logistic regression
           ``multinomial'': Multinomical logistic regression (This is currently not implemented)
    }

    \item{na.action}{
        A string which indicates what should happen when the data
        contain \code{NA}s. Possible
        values include \code{\link{na.omit}}, \code{"na.exclude"},
        \code{"na.fail"}
        and \code{NULL}. Right now, \code{\link{na.omit}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered on the MADlib side. User defined \code{na.action} function is allowed.
    }

    \item{control}{
        A list, extra parameters to be passed to linear or logistic
        regressions.

        For the linear regressions, the extra parameter is
        \code{hetero}. A logical, deafult is \code{FALSE}. If it is
  \code{TRUE}, then Breusch-Pagan test is performed on the fitting model
  and the corresponding test statistic and p-value are computed.

        For logistic regression, one can pass the following
        extra parameters:

        \code{method}: A string, default is \code{"irls"} (iteratively
        reweighted least squares [3]), other choices
        are \code{"cg"} (conjugate gradient descent algorithm [4]) and \code{"igd"}
        (stochastic gradient descent algorithm [5]). The algorithm names for
        logistic regression.

        \code{max.iter}: An integer, default is 10000. The maximum
        number of iterations that the algorithms will run.

        \code{tolerance}: A numeric value, default is 1e-5. The stopping
    threshold for the iteration algorithms.
    }

    \item{\dots}{
        Further arguments passed to or from other methods. Currently, no
        more parameters can be passed to the linear regression and
        logistic regression.
    }
}

\details{
  See \code{\link{madlib.lm}} for more details.
}

\value{
  \code{coef}:

  For ungrouped regressions, a named numeric vector giving the fitted coefficients.

    For grouped regressions, a list giving the coefficients for each of the component models.

\code{summary}:

    The function returns the \code{logregr.madlib} or \code{logregr.madlib.grps}
    object passed to the function

    \code{print} and \code{show}: No value is returned.

    \code{vcov}:

      For \code{logregr.madlib} objects, this function  returns the variance-covariance matrix of the main parameters.

For \code{logregr.madlib.grps} objects, which are a list of models for multiple groups of data, returns a list, each of which is the variance-cocariance matrix for the model of each group of data.

\code{groups}:

    A list that contains the value of each grouping colum. The elements
    of the list are the same as the grouping columns. If \code{x} is a
    \code{logregr.madlib} object with one group's information in it, the
    elements of the resulting list contain one value for each grouping
    column. If \code{x} is \code{logregr.madlib.grps}, which contains
    multiple groups' information, then each element of the resulting
    list is a vector with the length equal to the number of different
    groups.

    If no grouping column is used, this funcion returns \code{NULL}.

    \code{residuals}:

     For ungrouped regressions, \code{residuals} returns an object of class \code{\linkS4class{db.Rquery}}

    For grouped regressions, \code{residuals} returns a list of \code{\linkS4class{db.Rquery}} objects giving the output of these methods for each of the component models. Similarly, \code{AIC} for a grouped regression returns a vector of the AICs for each of the component models.

\code{logLik}, \code{AIC}, \code{extractAIC}:

For ungrouped regressions, \code{logLik} returns an object of
    class \code{logLik}, and \code{extractAIC} returns a length-2
    numeric vector giving the edf and AIC.

    For grouped regressions, \code{logLik} and \code{extractAIC} return
  a list giving the output of these methods for each of the component
  models. Similarly, \code{AIC} for a grouped regression returns a
  vector of the AICs for each of the component models.

  \code{predict}:

    A \code{\linkS4class{db.Rquery}} object, which contains the SQL query to compute the predictions.

\code{madlib.glm}:

  For the return value of linear regression see \code{\link{madlib.lm}}
  for details.

  For the logistic regression, the returned value is similar to that of
  the linear regression.  If there is no grouping (i.e. no \code{|} in the formula), the result
  is a \code{logregr.madlib} object. Otherwise, it is a
  \code{logregr.madlib.grps}
  object, which is just a list of \code{logregr.madlib} objects.

  A \code{logregr.madlib} object
  a list which contains the following items:

  \item{grouping column(s)}{
    When there are grouping columns in the formula, the resulting list
    has multiple items, each of which has the same name as one of the
    grouping columns. All of these items are vectors, and they have the
    same length, which is equal to the number of distinct combinations of
    all the grouping column values. Each row of these items together is
    one distinct combination of the grouping values. When there is no
    grouping column in the formula, none of such items will appear in the
    resulting list.
  }

  \item{coef}{
    A numeric matrix, the fitting coefficients. Each row contains the
    coefficients for the linear regression of each group of data. So the
    number of rows is equal to the number of distinct combinations of
    all the grouping column values.
  }

  \item{log_likelihood}{
    A numeric array, the log-likelihood for each fitting to the
    groups. Thus the length of the array is equal to \code{grps}.
  }

  \item{std_err}{
    A numeric matrix, the standard error for each coefficients. The row
    number is equal to \code{grps}.
  }

  \item{z_stats}{
    A numeric matrix, the z-statistics for each coefficient. Each row is
    for a
    fitting to a group of the data.
  }

  \item{p_values}{
    A numeric matrix, the p-values of \code{z_stats}. Each row is for a
    fitting to a group of the data.
  }

  \item{odds_ratios}{
    A numeric array, the odds ratios [6] for the fittings for all groups.
  }

  \item{condition_no}{
      A numeric array, the condition number for all combinations of the
    grouping column values.
  }

  \item{num_iterations}{
    An integer array, the itertion number used by each fitting group.
  }

  \item{grps}{
    An integer, the number of groups that the data is divided into
    according to the grouping columns in the formula.
  }

  \item{grp.cols}{
    An array of strings. The column names of the grouping columns.
  }

  \item{has.intercept}{
    A logical, whether the intercept is included in the fitting.
  }

  \item{ind.vars}{
       An array of strings, all the different terms used as independent
    variables in the fitting.
  }

  \item{ind.str}{
    A string. The independent variables in an array format string.
  }

  \item{call}{
    A language object. The function call that generates this result.
  }

  \item{col.name}{
    An array of strings. The column names used in the fitting.
  }

  \item{appear}{
       An array of strings, the same length as the number of independent
    variables. The strings are used to print a clean result, especially when
    we are dealing with the factor variables, where the dummy variable
    names can be very long due to the inserting of a random string to
    avoid naming conflicts, see \code{\link{as.factor,db.obj-method}}
    for details. The list also contains \code{dummy} and \code{dummy.expr}, which are also used for processing the categorical variables, but do not contain any important information.
  }

  \item{model}{
    A \code{\linkS4class{db.data.frame}} object, which wraps the result
    table of this function.
  }

  \item{terms}{
    A \code{\link{terms}} object, describing the terms in the model formula.
  }

  \item{nobs}{
    The number of observations used to fit the model.
  }

  \item{data}{A \code{db.obj} object, which wraps all the
    data used in the database. If there are fittings for multiple groups,   then this is only the wrapper for the data in one group.}

  \item{origin.data}{
  The original \code{db.obj} object. When there is no grouping, it is equal to \code{data} above, otherwise it is the "sum" of \code{data} from all groups.
}

  Note that if there is grouping done, and there are multiple
  \code{logregr.madlib} objects in the final result, each one of them
  contains the same copy \code{model}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. Hong Ooi at Pivotal Inc. contributed the code for \code{AIC}, \code{extractAIC}, \code{logLik}.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  See \code{\link{madlib.lm}}'s note for more about the formula format.

  For logistic regression, the dependent variable MUST be a logical
  variable with values being \code{TRUE} or \code{FALSE}.
}

\references{
    [1] Documentation of linear regression in MADlib v0.6,
    \url{http://doc.madlib.net/v0.6/group__grp__linreg.html}

    [2] Documentation of logistic regression in MADlib v0.6,
    \url{http://doc.madlib.net/v0.6/group__grp__logreg.html}

    [3] Wikipedia: Iteratively reweighted least squares,
    \url{http://en.wikipedia.org/wiki/IRLS}

    [4] Wikipedia: Conjugate gradient method,
    \url{http://en.wikipedia.org/wiki/Conjugate_gradient_method}

    [5] Wikipedia: Stochastic gradient descent,
    \url{http://en.wikipedia.org/wiki/Stochastic_gradient_descent}

    [6] Wikipedia: Odds ratio,
    \url{http://en.wikipedia.org/wiki/Odds_ratio}
}

\seealso{
    \code{\link{madlib.lm}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}} are MADlib
  wrapper functions.

  \code{\link{as.factor}} creates categorical variables for fitiing.

  \code{\link{delete}} safely deletes the result of this function.
}

\examples{
    \dontrun{
## connect to a data base
db.connect(dbname = "exampledb", user = "example", host = "localhost", port = 5433)
source_data <- db.data.frame("madlibtestdata.lin_ornstein")
preview(source_data, 10)

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.glm(interlocks ~ . | nation, data = source_data, heteroskedasticity = T)
fit

## logistic regression

## logistic regression
## The dependent variable must be a logical variable
## Here it is y < 10.
fit <- madlib.glm(interlocks < 10 ~ . - 1 , data = source_data)

fit <- madlib.glm(interlocks < 10 ~ assets + as.factor(nation) + sector,
data = source_data)

## 3rd example
## The table has two columns: x is an array, y is double precision
array.data <- db.data.frame("tbl_has_array_column")

## Fit to y using every element of x
## This does not work in R's lm, but works in madlib.lm
fit <- madlib.glm(y < 10 ~ x, data = array.data, family = "binomial")

fit <- madlib.glm(y < 10 ~ x - x[1:2], data = array.data, family = "binomial")

## 4th example
## Step-wise feature selection
dat <- as.db.data.frame(abalone, "abalone")
start <- madlib.glm(rings < 10 ~ . - id - sex, data = dat, family = "binomial")
step(start)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{madlib}
\keyword{stats}