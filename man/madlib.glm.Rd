\name{madlib.glm}
\alias{madlib.glm}

\title{Linear regression, Logistic Regression, and Multinomial Logistic Regression}

\description{
  The wrapper function for MADlib linear regression and logistic regression.
  Heteroskedasticity test is implemented for linear regression . One or multiple
  columns of data can be used to separate the data set into multiple groups
  according to the values of the grouping columns. The requested regression
  method is applied onto each group,  which has fixed values of the grouping
  columns.
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
        contain \code{NA}s. Currently not implemented yet. Future possible
        values include \code{"na.omit"}, \code{"na.exclude"}, \code{"na.fail"}
        and \code{NULL}.
    }

    \item{control}{
        Describe \code{control} here~~
    }

    \item{...}{
        Further arguments passed to or from other methods.
    }
}

\details{
If necessary, more details than the description above ~~
}

\value{
~~~~~~~~~~~
  A list which contains the following items:

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

  \item{r2}{
        A numeric vector, the coefficient of determination. Each element is
        the coefficient of determination of regression model for each group of data.
        It is a measure of how well observed outcomes are replicated by the model,
        as the proportion of total variation of outcomes explained by the model.
  }

  \item{std_err}{
    To be added ...
  }

  \item{t_stats}{
    To be added ...
  }

  \item{p_values}{
    To be added ...
  }

  \item{condition_no}{
    To be added ...
  }

  \item{bp_stats}{
    Only if ``gaussian'' family is used.
  }

  \item{bp_p_value}{
    Only if ``gaussian'' family is used.
  }

  \item{grps}{
    An integer, the number of groups that the data is divided into
    according to the grouping columns in the formula.
  }

  \item{grp.cols}{
    To be added ...
  }

  \item{has.intercept}{
    To be added ...
  }

  \item{ind.vars}{
    To be added ...
  }

  \item{ind.str}{
    To be added ...
  }

  \item{call}{
    To be added ...
  }

  \item{col.name}{
    To be added ...
  }

  \item{appear}{
    To be added ...
  }
  ~~~~~~~~~~~
}

\references{
    [1] Documentation of linear regression in MADlib v0.6,
        \url{http://doc.madlib.net/v0.6/group__grp__linreg.html}
    [2] Documentation of logistic regression in MADlib v0.6,
        \url{http://doc.madlib.net/v0.6/group__grp__logreg.html}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\note{
  \code{|} is not part of standard R formula object, but many R packages
  use \code{|} to add their own functionalities into formula
  object. However, \code{|} has different meanings and usages
  in different packages. The user must be careful that usage of \code{|} in
  \code{\link{PivotalR-package}} may not be the same as the others.
}

\seealso{
    \code{\link{madlib.lm}} Linear Regression
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
## create a table from the example data.frame
source_data <- db.data.frame("madlibtestdata.log_breast_cancer_wisconsin")
preview(source_data, 10)

## logistic regression
fit <- madlib.logregr(y ~ x - 1 , data = source_data)
fit
    }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ madlib }
\keyword{ wrapper }
\keyword{ regression }
