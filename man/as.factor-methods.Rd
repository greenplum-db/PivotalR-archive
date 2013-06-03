\name{as.factor}
\alias{as.factor,db.obj-method}

\title{ Convert one column of a \code{db.obj} object into a categorial variable }

\description{
  Convert one column of a \code{db.obj} object into a categorial
  variable. When \code{\link{madlib.lm}} or \code{\link{madlib.glm}} are
  applied onto a \code{db.obj} with categorial columns, dummy columns
  will be created and fitted.
}

\usage{
\S4method{as.factor}{db.obj}(x)
}

\arguments{
  \item{x}{
    A \code{db.obj} object. It must have only one column.
  }
}

\value{
  A \code{db.Rquery} object. It has only one column which is categorial.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{madlib.lm}} and \code{\link{madlib.glm}} can fit
  categorial variables

  When \code{\link{as.db.data.frame}} creates a table/view, it can create dummy variables for a
  categorial variable.
}

\examples{
\dontrun{
## get help for a method
help("as.factor,db.obj-method")

## Suppose that a valid connection with ID 1 is already created by db.connect

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone") # use default connection 1

## set sex to be a categorial variable
x$sex <- as.factor(x$sex)

fit1 <- madlib.lm(rings ~ . - id, data = x) # linear regression

fit2 <- madlib.glm(rings < 10 ~ . - id, data = x, family = "binomial") # logistic regression

## create another R db.data.frame object pointing to "abalone"
z <- db.data.frame("abalone", conn.id = 1) # explicitly specify the connection

## specify factor during fitting
fit3 <- madlib.lm(rings ~ as.factor(sex) + length + diameter, data = z)

fit4 <- madlib.glm(rings < 10 ~ as.factor(sex) + length + diameter, data
= z, family = "binomial")
}
}

\keyword{methods}
\keyword{ factor }
