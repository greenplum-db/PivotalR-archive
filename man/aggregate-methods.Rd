\name{Aggregate functions}

\alias{Aggregate functions}
\alias{mean,db.obj-method}
\alias{sum,db.obj-method}
\alias{length,db.obj-method}
\alias{max,db.obj-method}
\alias{min,db.obj-method}
\alias{sd,db.obj-method}
\alias{var,db.obj-method}
\alias{colMeans,db.obj-method}
\alias{colSums,db.obj-method}
\alias{c,db.obj-method}

\title{Functions to perform a calculation on multiple values and return a single value}
\description{
    An aggregate function is a function where the values of multiple rows are
grouped together as input to calculate a single value of more
significant meaning or measurement. The aggregate functions included are
mean, sum, count, max, min, standard deviation, and variance. Also included is
a function to compute the mean value of each column and a function to compute
the sum of each column.}

\usage{
\S4method{mean}{db.obj}(x, ...)

\S4method{sum}{db.obj}(x, ..., na.rm = FALSE)

\S4method{length}{db.obj}(x)

\S4method{max}{db.obj}(x, ..., na.rm = FALSE)

\S4method{min}{db.obj}(x, ..., na.rm = FALSE)

\S4method{sd}{db.obj}(x)

\S4method{var}{db.obj}(x)

\S4method{colMeans}{db.obj}(x, na.rm = FALSE, dims = 1, ...)

\S4method{colSums}{db.obj}(x, na.rm = FALSE, dims = 1, ...)

\S4method{c}{db.obj}(x, ..., recursive = FALSE)
}

\arguments{
    \item{x}{
        A \code{db.obj} object.
        The signature of the method.
    }

    \item{\dots}{
        further arguments passed to or from other methods
        This is currently not implemented.
    }

    \item{na.rm}{
        logical.  Should missing values (including 'NaN') be removed?
        This is currently not implemented.
    }

    \item{dims}{
        integer: Which dimensions are regarded as 'rows' or 'columns'
          to sum over.
          This is currently not implemented and the default behavior is to
          sum over columns
    }

    \item{recursive}{
        logical. This is currently not implemented.
    }

}

\details{
    For aggregate functions: mean, sum, length, max, min, sd, and var, the signature
    \code{x} must be a reference to a single column in a table.

    For aggregate functions: colMeans, colSums, and c the signature
    \code{x} can be a \code{db.obj} referencing to a single column or a single
    table, or can be a \code{db.Rquery} referencing to multiple columns in a table.
}

\value{
    For \code{mean}, a \code{db.Rquery} which is a SQL query to extract the average of a column of a table.

    For \code{sum}, a \code{db.Rquery} which is a SQL query to extract the sum of a column of a table.

    For \code{length}, a \code{db.Rquery} which is a SQL query to extract the count of a column of a table.

    For \code{max}, a \code{db.Rquery} which is a SQL query to extract the max of a column of a table.

    For \code{min}, a \code{db.Rquery} which is a SQL query to extract the min of a column of a table.

    For \code{sd}, a \code{db.Rquery} which is a SQL query to extract the standard deviation of a column of a table.

    For \code{var}, a \code{db.Rquery} which is a SQL query to extract the variance of a column of a table.

    For \code{colMeans}, a \code{db.Rquery} which is a SQL query to extract the mean of multiple columns of a table.

    For \code{colSums}, a \code{db.Rquery} which is a SQL query to extract the sum of multiple columns of a table.

    For \code{c}, a \code{db.Rquery} which is a SQL query to extract the values as an array aggregate.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{by,db.obj-method}} is usually used together with
    aggregate functions.
}

\examples{
\dontrun{
## get the help for a method
help("mean,db.obj-method")

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

# get the array aggregate of a specific column in the table
c(x$diameter)

# get the array aggregate of all columns in the table
c(x)
    }
}

\keyword{database}
\keyword{aggregates}
