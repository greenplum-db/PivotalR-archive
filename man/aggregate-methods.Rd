\name{Aggregate functions}

\alias{mean}
\alias{sum}
\alias{length}
\alias{max}
\alias{min}
\alias{sd}
\alias{var}
\alias{colMeans}
\alias{colSums}
\alias{c}


\title{Functions to perform a calculation on multiple values and return a single value}
\description{
    An aggregate function is a function where the values of multiple rows are
grouped together as input on certain criteria to calculate a single value of more
significant meaning or measurement. The aggregate functions included here are
mean, sum, count, max, min, standard deviation, and variance. The set also
includes a function to compute mean value of each column and a function to compute
the sum of each column.}

\usage{
    mean(y$rings)

    sum(y$rings)

    length(y$rings)

    max(y$rings)

    min(y$rings)

    sd(y$rings)

    var(y$rings)

    colMeans(y)

    colSums(y)
}

\arguments{
    \item{x}{
        The signature of the method.
        A \code{db.obj} object, which points to either a table or a column of a
        table in the database.
    }
}

\details{
    For aggregates mean, sum, length, max, min, sd, and var, the signature
    \code{x} must be a reference to a column in a table.

    For aggregates colMeans and colSums, the signature
    \code{x} can be a reference to a single column or the table.
}

\value{
    For \code{mean}, a \code{db.Rquery} which is a SQL query to extract the average of a column of a table.

    For \code{sum}, a \code{db.Rquery} which is a SQL query to extract the sum of a column of a table..

    For \code{length}, a \code{db.Rquery} which is a SQL query to extract the count of a column of a table..

    For \code{max}, a \code{db.Rquery} which is a SQL query to extract the max of a column of a table..

    For \code{min}, a \code{db.Rquery} which is a SQL query to extract the min of a column of a table..

    For \code{sd}, a \code{db.Rquery} which is a SQL query to extract the standard deviation of a column of a table..

    For \code{var}, a \code{db.Rquery} which is a SQL query to extract the variance of a column of a table..

    For \code{colMeans}, a \code{db.Rquery} which is a SQL query to extract the mean of multiple columns of a table.

    For \code{colSums}, a \code{db.Rquery} which is a SQL query to extract the sum of multiple columns of a table.

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

% \seealso{
%     \code{\link{}} description
% }

\examples{
    \dontrun{
## Suppose that a valid connection to a database (with ID=1) is established
## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

# get the mean of a column
mean(x$diameter)

# get the sum of a column
sum(x$height)

# get the number of entries in a column
length(x$id)

# get the max value of a column
max(x$diameter)

# get the min value of a column
min(x$diameter)

# get the standard deviation of the values in column
sd(x$diameter)

# get the variance of the values in column
var(x$diameter)

# get the mean of all columns in the table
colMeans(x)

# get the sum of all columns in the table
colSums(x)
    }
}

\keyword{database}
\keyword{aggregates}
\keyword{utility}