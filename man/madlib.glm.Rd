\name{madlib.glm}
\alias{madlib.glm}

\title{Linear regression, Logistic Regression, and Multinomial Logistic Regression}

\description{
  The wrapper function for MADlib linear regression and logistic regression.
  Heteroskedasticity test is implemented for linear regression . One or multiple
  columns of data can be used to separate the data set into multiple groups
  according to the values of the grouping columns. The requested regression
  method is applied onto each group,  which has fixed values of the grouping
  columns. Multinomial logistic regression is not implemented
  yet. Categorial variables are supported. The
  computation is parallelized by MADlib if the connected database is
  Greenplum database. The regression computation can also be done on a
  column that is an array in the data table.
}

\usage{
    madlib.glm(formula, data, family = "gaussian", na.action, control = list(), ...)
}

\arguments{
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
        contain \code{NA}s.  Future possible
        values include \code{"na.omit"}, \code{"na.exclude"},
        \code{"na.fail"} 
        and \code{NULL}. Currently not implemented yet.
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
        are \code{"cgd"} (conjugate gradient descent algorithm [4]) and \code{"igd"}
        (stochastic gradient descent algorithm [5]). The algorithm names for
        logistic regression.

        \code{max_iter}: An integer, default is 10000. The maximum
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
    for details.
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

  \item{data}{The original \code{db.obj} object, which wraps all the
  data used in the database.}

  Note that if there is grouping done, and there are multiple
  \code{logregr.madlib} objects in the final result, each one of them
  contains the same copy \code{model}.
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

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\note{
  See \code{\link{madlib.lm}}'s note for more about the formula format.

  For logistic regression, the dependent variable MUST be a logical
  variable with values being \code{TRUE} or \code{FALSE}.
}

\seealso{
    \code{\link{madlib.lm}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}} are MADlib
  wrapper functions.

  \code{\link{as.factor}} creates categorial variables for fitiing.

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
fit <- madlib.lm(y < 10 ~ x, data = array.data)
    }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{madlib}
\keyword{stats}

