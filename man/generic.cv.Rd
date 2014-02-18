\name{generic.cv}
\alias{generic.cv}

\title{
  Generic cross-validation for supervised learning algorithms
}

\description{
  This function runs cross-validation for a given supervised learning
  model, which is specified by the training function, prediction
  function, and metric function. The user might need to write wrappers
  for the functions so that they satisfy the format requirements
  desceribed in the following. This function works on both in-memory and in-database data.
}

\usage{
generic.cv(train, predict, metric, data, params = NULL, k = 10,
approx.cut = TRUE, verbose = TRUE, find.min = TRUE)
}

\arguments{
  \item{train}{
    A training function. Its first argument must be a \code{\linkS4class{db.obj}} object which is the wrapper for the data in database.
    Given the data, it produces the model. It can also have
  other parameters that specifies the model, and these parameters must
  appear in the list \code{params}.
  }

  \item{predict}{
    A prediction function. It must have only two arguments, which are the fitted model (the first argument) and
    the new data input for prediction (the second argument).
  }

  \item{metric}{
    A metric function. It must have only two arguments.
    The first argument is the
    prediction and the second is the data that contains the actual value. This function shoud measure the difference between the predicted and actual values and produce a single numeric value.
  }

  \item{data}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the
    database, used for cross-validation. Or a \code{data.frame}, which contains data in memory.
  }

  \item{params}{
    A list, default is \code{NULL}. The values of each parameters used
    by the training function. An array of values for each parameter is
    an element
    in the list. The value arrays for different parameters do not have
    to be the same length. The arrays of shorter lengths are circularly
    expanded to the length of the longest element.
  }

  \item{k}{
    An integer, default is 10. The cross-validation fold number.
  }

  \item{approx.cut}{
    A boolean, default is TRUE. Whether to cut the data into \code{k} pieces in an approximate way, which is faster than the accurate way. For big data sets, cutting the data into k pieces in an approximate way does not affect the result. See details for more.
  }

  \item{verbose}{
    A logical value, default is \code{TRUE}. Whether to print
  }

  \item{find.min}{
    A logical value, default is \code{TRUE}. Whether the best set of parameters produces the mode with the minimum metric value. Then a model will be trained on the whole data set using the best set of parameters. If it is \code{FALSE}, the parameter set with the maximum metric value will be used. This is ignored if \code{params} is \code{NULL}.
  }
}

\details{
  In order to cut the data table into \code{k} equal pieces, a column of unique id for every row needs to be attached to the data so that one can cut the data using different ranges of the row id. For example, for a 1000 rows data table, when id is 1-100, 101-200, ..., one can cut the data into 10 pieces. The id should be randomly assigned to the rows for cross-validation to use. Note that the original data is not touched in this process, instead all the data is copied to a new temporary table with the id column created in the new table. Because a unique id is to be randomly assigned to each row, this process cannot be easily parallelized.

When \code{approx.cut} is \code{TRUE}, which is the default, a column of uniform random integer instead of consecutive integers is created in the temporary table. We apply the same method to cut the data using the different ranges of this column, for example, 1-100, 101-200, etc. Apparently, the \code{k} pieces of data do not have an exact equal size, and the sizes of them are only approximately equal. However, for big data sets, the differeces are relatively small and should not affect the result. This process does not generate unique ID's for the rows, but can be easily parallelized, so this method is much faster for big data sets.
}

\value{
  If \code{params} is \code{NULL}, this function returns
  a \code{list}, which contains two elements: \code{err} and
  \code{err.std}, which are the errors and its standard deviation.

  If \code{params} is not \code{NULL}, this function returns a \code{cv.generic} object, which is a \code{list} that contains the following items:

\item{metric}{
  A list, which contains

  - \code{avg} The average metric value for each set of parameters.

  - \code{std} The standard deviation for the metric values of each set of parameters.

- \code{vals} A matrix that contains all the metric value measured, whose rows correspond to different set of parameters, and columns correspond to different folds of cross-validation.
}

\item{params}{
  A data.frame, which contains all the parameter sets.
}

\item{best}{
  The fit that has the optimum metric value.
}

\item{best.params}{
  A list, the set of parameters that produces the optimum metric value.
}

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{generic.bagging}} does the boostrap aggregate computation.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name

## set up the database connection
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## ----------------------------------------------------------------------

dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

err <- generic.cv(     function(data) {
        madlib.lm(rings ~ . - id - sex, data = data)
    },
    predict,
    function(predicted, data) {
        lookat(mean((data$rings - predicted)^2))
    }, data = dat, verbose = FALSE)

## ----------------------------------------------------------------------

x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100, 0.1, 2)

dat <- data.frame(x, y)
delete("eldata", conn.id = cid)
z <- as.db.data.frame(dat, "eldata", conn.id = cid, verbose = FALSE)

g <- generic.cv(
    train = function (data, alpha, lambda) {
        madlib.elnet(y ~ ., data = data, family = "gaussian",
        alpha = alpha, lambda = lambda,
        control = list(random.stepsize=TRUE))
    },
    predict = predict,
    metric = function (predicted, data) {
        lk(mean((data$y - predicted)^2))
    },
    data = z,
    params = list(alpha=1, lambda=seq(0,0.2,0.1)),
    k = 5, find.min = TRUE, verbose = FALSE)

plot(g$params$lambda, g$metric$avg, type = 'b')

g$best

## ----------------------------------------------------------------------

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{stats}
\keyword{math}
