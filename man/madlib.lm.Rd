\name{madlib.lm}

\alias{madlib.lm}

\title{
  Linear regression with grouping support, heteroskedasticity 
}

\description{
  The wrapper function for MADlib linear regression. Heteroskedasticity
  can be detected using Breusch-Pagan test. One or multiple columns of
  data can be used to separated the data set into multiple groups
  according to the values of the grouping columns. Linear regression is
  applied onto each group, which has fixed values of the grouping columns.
}

\usage{
madlib.lm(formula, data, na.action, hetero = FALSE, ...)
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
    contain \code{NA}s. Currently not implemented yet. Future possible
  values include \code{"na.omit"}, \code{"na.exclude"}, \code{"na.fail"}
  and \code{NULL}.
  }

  \item{hetero}{
    A boolean value with default value \code{FALSE}. If it is
  \code{TRUE}, then Breusch-Pagan test is performed on the fitting model
  and the corresponding test statistic and p-value are computed. See [1]
  for more details.
  }

  \item{...}{
    More parameters can be passed into this function. Currently, it is
  just a place holder and any parameter here is not used.
  }
}

\details{
  To be added ...
}

\value{
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
    To be added ...
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
    To be added ...
  }

  \item{bp_p_value}{
    To be added ...
  }

  \item{grps}{
    To be added ...
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

  \item{call}{
    To be added ...
  }
}

\references{
  [1] Wikipedia: Breusch-Pagan test,
  \url{http://en.wikipedia.org/wiki/Breusch-Pagan_test}
  [2] Documentation of linear regression in MADlib v0.6,
  \url{http://doc.madlib.net/v0.6/group__grp__linreg.html}.
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
  \code{\link{madlib.glm}} logistic regression 
}
\examples{
\dontrun{

## connect to a data base
db.connect(dbname = "exampledb", user = "example", host = "localhost", port = 5433)
x <- db.data.frame("madlibtestdata.lin_ornstein")
preview(x, 10)

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.lm(interlocks ~ . | nation, data = x, heteroskedasticity = T)
fit

## use I(.) for expressions
fit <- madlib.lm(interlocks ~ assets + sector + nation + I(assets^2),
data = x, heteroskedasticity = T)
fit
}
}

\keyword{ madlib }
\keyword{ wrapper }
