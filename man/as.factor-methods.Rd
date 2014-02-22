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
## help("as.factor,db.obj-method")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a temporary table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## set sex to be a categorical variable
x$sex <- as.factor(x$sex)

fit1 <- madlib.lm(rings ~ . - id, data = x) # linear regression

fit2 <- madlib.glm(rings < 10 ~ . - id, data = x, family = "binomial") # logistic regression

## another temporary table
z <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## specify factor during fitting
fit3 <- madlib.lm(rings ~ as.factor(sex) + length + diameter, data = z)

## as.factor is automatically used onto text column
## so as.factor is not necessary
fit4 <- madlib.glm(rings < 10 ~ sex + length + diameter, data
= z, family = "binomial")

## using relevel to change the reference level
x$sex <- relevel(x$sex, ref = "M")
madlib.lm(rings ~ . - id, data = x) # use "M" as the reference level

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{factor}
\keyword{math}