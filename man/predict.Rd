\name{predict}
\alias{predict.lm.madlib}
\alias{predict.logregr.madlib}
\alias{predict.lm.madlib.grps}
\alias{predict.logregr.madlib.grps}

\title{
  Generate the \code{db.Rquery} object that can calculate the predictions
}

\description{
  Generate the \code{db.Rquery} object that can calculate the
  predictions for linear/logistic regressions. The actual result can be
  viewed using \code{\link{preview}}.
}
\usage{
\method{predict}{lm.madlib}(object, newdata, ...)

\method{predict}{lm.madlib.grps}(object, newdata, ...)

\method{predict}{logregr.madlib}(object, newdata, ...)

\method{predict}{logregr.madlib.grps}(object, newdata, ...)
}

\arguments{
  \item{object}{
    The
  result of \code{\link{madlib.lm}} and \code{\link{madlib.glm}}.
  }
  
  \item{newdata}{
    A \code{db.obj} object, which contains the information about the
    real data in the database.
  }
  
  \item{\dots}{
    Extra parameters. Not implemented yet.
  }
}

\value{
  A \code{\linkS4class{db.Rquery}} object, which contains the SQL query to compute the
    predictions. 
}

\author{
 Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.lm}} linear regression

  \code{\link{madlib.glm}} logistic regression

  \code{\link{preview}} view the actual result

  \code{\link{groups.lm.madlib}}, \code{\link{groups.lm.madlib.grps}},
  \code{\link{groups.logregr.madlib}},
  \code{\link{groups.logregr.madlib.grps}} extract grouping column
  information from the fitted model(s). 
}
\examples{
\dontrun{
db.connect(port = 5433)

## create db.table object pointing to a data table
delete("abalone")
x <- as.db.data.frame(abalone, "abalone")

## Example 1 --------

fit <- madlib.lm(rings ~ . - sex - id, data = x)

fit

pred <- predict(fit, x) # prediction

content(pred)

ans <- x$rings # the actual value

preview((ans - pred)^2, 10) # squared error

preview(mean((ans - pred)^2)) # mean squared error

## Example 2 ---------

y <- x
y$sex <- as.factor(y$sex)
fit <- madlib.lm(rings ~ . - id, data = y)

preview(mean((y$rings - predict(fit, y))^2))

## Example 3 ---------

fit <- madlib.lm(rings ~ . - id | sex, data = x)

fit

pred <- predict(fit, x)

content(pred)

ans <- x$rings

preview(mean((ans - pred)^2))

## predictions for one group of data where sex = I
idx <- which(groups(fit)[["sex"]] == "I") # which sub-model
pred1 <- predict(fit[[idx]], x[x$sex == "I",]) # predict on part of data

## Example 3 --------

## plot the predicted values v.s. the true values
ap <- ans # true values
ap$pred <- pred # add a column which is the predicted values

## If the data set is very big, you do not want to load all the 
## data points into R and plot. We can just plot a random sample.
random.sample <- preview(sort(ap, FALSE, NULL), 1000) # sort randomly

plot(random.sample)
}
}

\keyword{ methods }
\keyword{ math }
\keyword{ stats }