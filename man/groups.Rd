\name{groups}
\alias{groups}
\alias{groups.lm.madlib}
\alias{groups.logregr.madlib}
\alias{groups.lm.madlib.grps}
\alias{groups.logregr.madlib.grps}

\title{Summary information for Logistic Regression output}
\description{
  The function prints the value of each element in the Logistic
  Regression output object.
}

\usage{
\method{groups}{lm.madlib}(x)

\method{groups}{lm.madlib.grps}(x)

\method{groups}{logregr.madlib}(x)

\method{groups}{logregr.madlib.grps}(x)
}

\arguments{
  \item{x}{
    The result of \code{\link{madlib.lm}} or
    \code{\link{madlib.glm}}
  }
}

\value{
    A list that contains the value of each grouping colum. The elements
    of the list are the same as the grouping columns. If \code{x} is a
    \code{lm.madlib} object with one group's information in it, the
    elements of the resulting list contain one value for each grouping
    column. If \code{x} is \code{lm.madlib.grps}, which contains
    multiple groups' information, then each element of the resulting
    list is a vector with the length equal to the number of different
    groups. \code{logregr.madlib} and \code{logregr.madlib.grps} have
    the similar interpretation of the results.

    If no grouping column is used, this funcion returns \code{NULL}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.glm}} wrapper for MADlib linear and logistic regressions.

  \code{\link{madlib.lm}} wrapper for MADlib linear regression

  \code{\link{predict.lm.madlib}}, \code{\link{predict.lm.madlib.grps}},
  \code{\link{predict.logregr.madlib}},
  \code{\link{predict.logregr.madlib.grps}} make
  predictions for new data.
}


\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame
delete("abalone", conn.id = cid)
source_data <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)
lk(source_data, 10)

## logistic regression
fit <- madlib.glm(rings < 10 ~ . - id | sex , data = source_data, family = "binomial")

groups(fit) # all grouping column values

groups(fit[[1]]) # the first model's grouping column value

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{madlib}
\keyword{math}
\keyword{stats}