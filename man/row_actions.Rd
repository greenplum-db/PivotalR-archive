\name{Row_actions}

\alias{rowSums,db.obj-method}
\alias{rowMeans,db.obj-method}

\title{Compute the sum or mean of all columns in one row of a table}

\description{
  This function returns a \code{\linkS4class{db.Rquery}} object, which produces the sum or mean value of all columns of one row when executed in database.
}

\usage{
\S4method{rowSums}{db.obj}(x, na.rm = FALSE, dims = 1, ...)
\S4method{rowMeans}{db.obj}(x, na.rm = FALSE, dims = 1, ...)
}

\arguments{
    \item{x}{
      A \code{db.obj} object, which has only one column. The column can be casted into boolean values.
    }

    \item{na.rm}{
      logical.  Should missing values (including 'NaN') be omitted
      from the calculations? Not implemented yet.
    }

    \item{dims}{
       integer: Which dimensions are regarded as 'rows' or 'columns'
          to sum over.  For 'row*', the sum or mean is over dimensions
          'dims+1, ...'; for 'col*' it is over dimensions '1:dims'.
          Not implemented yet.
    }

    \item{\dots}{
      Other arguments. Not implemented yet.
    }
}

\value{
  A \code{\linkS4class{db.Rquery}} object which, when executed, computes the mean or sum of all columns on every row of a table.
}

\author{
  Author: Hong Ooi, Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{sum,db.obj-method}}, \code{\link{colSums,db.obj-method}}   compute the sum of each column.

\code{\link{mean,db.obj-method}}, \code{\link{colMeans,db.obj-method}} compute the mean values column-wise.
}

\examples{ \dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

row.sum <- rowSums(x[,-2]) # the second column is text
row.avg <- rowMeans(x[,-2])

## lokk at 10 results
lk(row.sum, 10)
lk(row.avg, 10)

db.disconnect(cid, verbose = FALSE)
}
}