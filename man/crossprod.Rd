\name{crossprod}

\alias{crossprod}
\alias{crossprod,db.obj-method}

\title{
  Compute the matrix product of \code{X^T} and \code{Y}.
}
\description{
  The function computes the cross product of two matrices. The matrix is
  stored in the table either as multiple columns of data or a column of
  arrays.  
}

\usage{
\S4method{crossprod}{db.obj}(x, y = x)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object. It either has multiple columns
  or a column of arrays, and thus forms a matrix.
  }
  \item{y}{
    A \code{\linkS4class{db.obj}} object, default is the same as
  \code{x}. This represents the second matrix in the cross product.
  }
 }

\value{
  \code{\linkS4class{db.Rcrossprod}} object, which is subclass of
  \code{\linkS4class{db.Rquery}}. It is actually a vectorized version of
  the resulting product matrix represented in an array. If you want to
  take a look at the actual values inside this matrix,
  \code{\link{preview}} or \code{\link{lookat}} can be used to extract
  the correct matrix format as long as the matrix can be loaded into the
  memory. Usually the resulting product matrix is not too large because
  the number \code{n} of columns is usually not too large and the
  dimension of the resulting matrix is \code{n x n}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{rowAgg}} forms an array using columns
}
\examples{
\dontrun{
## get the help for a method
help("crossprod,db.obj-method")

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

lookat(crossprod(x[,-c(1,2)]))

x$arr <- rowAgg(1, x$length, x$diameter)

lookat(crossprod(x$arr))
}
}

\keyword{methods}
\keyword{database}
\keyword{utility}
