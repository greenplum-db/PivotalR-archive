\name{madlib.lm}

\alias{madlib.lm}

\title{
  Linear regression with grouping support, heteroskedasticity
}

\description{
  The wrapper function for MADlib linear regression. Heteroskedasticity
  can be detected using the Breusch-Pagan test. One or multiple columns of
  data can be used to separated the data set into multiple groups
  according to the values of the grouping columns. Linear regression is
  applied onto each group, which has fixed values of the grouping
  columns. Categorial variables are supported, see details below. The
  computation is parallelized by MADlib if the connected database is
  Greenplum database. The regression computation can also be done on a
  column that is an array in the data table.
}

\usage{
madlib.lm(formula, data, na.action = NULL, hetero = FALSE, na.as.level = FALSE, ...)
}

\arguments{
  \item{formula}{
    an object of class "\code{\link{formula}}" (or one that can be coerced to
    that class): a symbolic description of the model to be
    fitted. The details of model specification are given under
    'Details'.
  }

  \item{data}{
    An object of \code{db.obj} class. Currently, this parameter is
  mandatory. If it is an object of class \code{db.Rquery} or
  \code{db.view}, a temporary table will be created, and further
  computation will be done on the temporary table. After the
  computation, the temporary will be dropped from the corresponding
  database.
  }

  \item{na.action}{
    A string which indicates what should happen when the data
    contain \code{NA}s. Possible
  values include \code{\link{na.omit}}, \code{"na.exclude"}, \code{"na.fail"}
  and \code{NULL}. Right now, \code{\link{na.omit}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered on the MADlib side. User defined \code{na.action} function is allowed.
  }

  \item{hetero}{
    A logical value with default value \code{FALSE}. If it is
  \code{TRUE}, then Breusch-Pagan test is performed on the fitting model
  and the corresponding test statistic and p-value are computed. See [1]
  for more details.
  }

  \item{na.as.level}{
      A logical value, default is \code{FALSE}. Whether to treat \code{NA}
      value as a level in a categorical variable or just ignore it.
  }

  \item{\dots}{
    More parameters can be passed into this function. Currently, it is
    just a place holder and any parameter here is not used.
  }
}

\details{
  For details about how to write a formula, see \code{\link{formula}}
  for details. "|" can be used at the end of the formula to denote that
  the fitting is done conditioned on the values of one or more
  variables. For example, \code{y ~ x + sin(z) | v + w} will do the
  fitting each distinct combination of the values of \code{v} and
  \code{w}.

  Both the linear regression (this function) and the logistic regression
  (\code{\link{madlib.glm}}) support categorical variables. Use
  \code{\link{as.factor,db.obj-method}} to denote that a variable is categorical, and
  the corresponding dummy variables are created and fitted. See
  \code{\link{as.factor,db.obj-method}} for more.
}

\value{
  If there is no grouping (i.e. no \code{|} in the formula), the result
  is a \code{lm.madlib} object. Otherwise, it is a \code{lm.madlib.grps}
  object, which is just a list of \code{lm.madlib} objects.

  A \code{lm.madlib} object is a list which contains the following items:

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
    all the grouping column values. The number of columns is equal to
    the number features (including intercept if it presents in the
    formula).
  }

  \item{r2}{
    A numeric array. R2 values for all combinations of the grouping
    column values.
  }

  \item{std_err}{
    A numeric matrix, the standard error for each coefficients.
  }

  \item{t_stats}{
    A numeric matrix, the t-statistics for each coefficient, which is
    the absolute value of the ratio of \code{std_err} and \code{coef}.
  }

  \item{p_values}{
    A numeric matrix, the p-values of \code{t_stats}. Each row is for a
    fitting to a group of the data.
  }

  \item{condition_no}{
    A numeric array, the condition number for all combinations of the
    grouping column values.
  }

  \item{bp_stats}{
    A numeric array when \code{hetero = TRUE}, the Breusch-Pagan test
    statistics for each combination of the grouping column values.
  }

  \item{bp_p_value}{
    A numeric array when \code{hetero = TRUE}, the Breusch-Pagan test p
    value for each combination of the grouping column values.
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

  \item{terms}{ A \code{\link{terms}} object, describing the terms in
    the model formula.  }

  \item{nobs}{
    The number of observations used to fit the model.
  }

  \item{data}{A \code{db.obj} object, which wraps all the
  data used in the database. If there are fittings for multiple groups,   then this is only the wrapper for the data in one group.}

\item{origin.data}{
  The original \code{db.obj} object. When there is no grouping, it is equal to \code{data} above, otherwise it is the "sum" of \code{data} from all groups.
}

  Note that if there is grouping done, and there are multiple
  \code{lm.madlib} objects in the final result, each one of them
  contains the same copy \code{model}.
}


\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}
\references{
  [1] Wikipedia: Breusch-Pagan test,
  \url{http://en.wikipedia.org/wiki/Breusch-Pagan_test}
  [2] Documentation of linear regression in MADlib v0.6,
  \url{http://doc.madlib.net/v0.6/group__grp__linreg.html}.
}
\note{
  \code{|} is not part of standard R formula object, but many R packages
  use \code{|} to add their own functionalities into formula
  object. However, \code{|} has different meanings and usages
  in different packages. The user must be careful that usage of \code{|} in
  \code{\link{PivotalR-package}} may not be the same as the others.
}

\seealso{
  \code{\link{madlib.glm}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}} are MADlib
  wrapper functions.

  \code{\link{as.factor}} creates categorical variables for fitiing.

  \code{\link{delete}} safely deletes the result of this function.
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

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.lm(rings ~ . - id | sex, data = x, heteroskedasticity = T)
fit

## use I(.) for expressions
fit <- madlib.lm(rings ~ length + diameter + shell + I(diameter^2),
data = x, heteroskedasticity = T)
fit # display the result

## Another example
fit <- madlib.lm(rings ~ . - id | sex + (id < 2000), data = x)

## 3rd example
## The table has two columns: x is an array, y is double precision
dat <- x
dat$arr <- db.array(x[,-c(1,2)])
array.data <- as.db.data.frame(dat)

## Fit to y using every element of x
## This does not work in R's lm, but works in madlib.lm
fit <- madlib.lm(rings ~ arr, data = array.data)

fit <- madlib.lm(rings ~ arr - arr[1], data = array.data)

fit <- madlib.lm(rings ~ . - arr[1:2], data = array.data)

fit <- madlib.lm(as.integer(rings < 10) ~ . - arr[1:2], data = array.data)

## 4th example
## Step-wise feature selection
start <- madlib.lm(rings ~ . - id - sex, data = x)
## step(start)

db.disconnect(cid)
}
}

\keyword{madlib}
\keyword{stats}
