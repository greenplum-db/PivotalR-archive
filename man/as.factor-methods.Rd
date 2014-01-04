\name{as.factor-methods}
\alias{relevel,db.obj-method}
\alias{as.factor,db.obj-method}

\title{ Convert one column of a \code{\linkS4class{db.obj}} object into a categorical variable }

\description{
  Convert one column of a \code{db.obj} object into a categorical
  variable. When \code{\link{madlib.lm}} or \code{\link{madlib.glm}} are
  applied onto a \code{db.obj} with categorical columns, dummy columns
  will be created and fitted. The reference level for regressions can be selected using \code{relevel}.
}

\usage{
\S4method{as.factor}{db.obj}(x)

\S4method{relevel}{db.obj}(x, ref, ...)
}

\arguments{
  \item{x}{
    A \code{db.obj} object. It must have only one column.
  }

  \item{ref}{
    A single value, which is the reference level that is used in the regressions.
  }

  \item{\dots}{
    Other arguments passed into the result. Not implemented yet.
  }
}

\value{
  A \code{db.Rquery} object. It has only one column which is categorical. By default, a reference level is automatically selected in regressions, which is usually the minimum of all levels, but one can easily change the reference level using \code{relevel}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.lm}} and \code{\link{madlib.glm}} can fit
  categorical variables

  When \code{\link{as.db.data.frame}} creates a table/view, it can create dummy variables for a
  categorical variable.
}

\examples{
\dontrun{
## get help for a method
help("as.factor,db.obj-method")

## Suppose that a valid connection with ID 1 is already created by db.connect

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone") # use default connection 1

## set sex to be a categorical variable
x$sex <- as.factor(x$sex)

fit1 <- madlib.lm(rings ~ . - id, data = x) # linear regression

fit2 <- madlib.glm(rings < 10 ~ . - id, data = x, family = "binomial") # logistic regression

## create another R db.data.frame object pointing to "abalone"
z <- db.data.frame("abalone", conn.id = 1) # explicitly specify the connection

## specify factor during fitting
fit3 <- madlib.lm(rings ~ as.factor(sex) + length + diameter, data = z)

fit4 <- madlib.glm(rings < 10 ~ as.factor(sex) + length + diameter, data
= z, family = "binomial")

## using relevel to change the reference level
x$sex <- relevel(x$sex, ref = "M")
madlib.lm(rings ~ . - id, data = x) # use "M" as the reference level
}
}

\keyword{methods}
\keyword{factor}
\keyword{math}