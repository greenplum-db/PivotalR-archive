\name{crossprod}

\alias{crossprod}
\alias{crossprod,db.obj,ANY-method}

\title{
  Compute the matrix product of \code{X^T} and \code{Y}.
}
\description{
  The function computes the cross product of two matrices. The matrix is
  stored in the table either as multiple columns of data or a column of
  arrays.
}

\usage{
\S4method{crossprod}{db.obj,ANY}(x, y = x)
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
  \code{\link{lk}} or \code{\link{lookat}} can be used to extract
  the correct matrix format as long as the matrix can be loaded into the
  memory. Usually the resulting product matrix is not too large because
  the number \code{n} of columns is usually not too large and the
  dimension of the resulting matrix is \code{n x n}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.array}} forms an array using columns
}
\examples{
\dontrun{
## get the help for a method
## help("crossprod,db.obj-method")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)

lookat(crossprod(x[,-c(1,2)]))

x$arr <- db.array(1, x$length, x$diameter)

lookat(crossprod(x$arr))

## -----------------------------------------------------

## Create a function that does Principal Component Analysis in parallel.
## As long as the number of features of the data table is fewer than
## ~ 5000, the matrix t(x) %*% x can be loaded into memory to compute
## the eigenvalues and eigenvectors. However, the step t(x) %*% x must
## be done in-database in parallel, because x can be very big.
pca <- function (x, center = TRUE, scale = FALSE)
{
    y <- scale(x, center = center, scale = scale) # centering and scaling
    z <- as.db.data.frame(y, verbose = FALSE) # create an intermediate table to save computation
    m <- lookat(crossprod(z)) # one scan of the table to compute Z^T * Z
    d <- delete(z) # delete the intermediate table
    res <- eigen(m) # only this computation is in R
    n <- attr(y, "row.number") # save the computation to count rows

    ## return the result
    list(val = sqrt(res$values/(n-1)), # eigenvalues
         vec = res$vectors, # columns of this matrix are eigenvectors
         center = attr(y, "scaled:center"),
         scale = attr(y, "scaled:scale"))
}

## create a data table with a random name
dat <- db.data.frame("abalone", conn.id = cid, verbose = FALSE)

## exclude id and sex columns
p <- pca(dat[,-c(1,2)])

p$val # eigenvalues

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{math}
