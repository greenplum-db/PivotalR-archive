\name{madlib.kmeans}
\alias{madlib.kmeans}

\title{Wrapper for MADlib's Kmeans clustering function}


\description{
  The wrapper function for MADlib's kmeans clustering [1]. Clustering refers
  to the problem of partitioning a set of objects according to some
  problem-dependent measure of similarity. Each centroid represents a cluster
  that consists of all points to which this centroid is closest. The
  computation is parallelized by MADlib if the connected database is
  Greenplum/HAWQ database.

}


\usage{

madlib.kmeans(
  x, centers, iter.max = 10, nstart = 1, algorithm = "Lloyd", key,
  fn.dist = "squared_dist_norm2", agg.centroid = "avg", min.frac = 0.001,
  kmeanspp = FALSE, seeding.sample.ratio=1.0, ...)

}

\arguments{

  \item{x}{
    An object of \code{db.obj} class. Currently, this parameter is
    mandatory. If it is an object of class \code{db.Rquery} or
    \code{db.view}, a temporary table will be created, and further
    computation will be done on the temporary table. After the computation,
    the temporary will be dropped from the corresponding database.
  }

  \item{centers}{

    A number, a matrix or db.data.frame object. If it is a number, this sets
    the number of target centroids and the random (or kmeans++) seeding method
    is used. Otherwise, this parameter is used for initial centers. If it is a
    matrix, its rows will denote the initial centroid coordinates. Else, this
    parameter will point to a table in the connected database that contains the
    initial centroids.
  }

  \item{iter.max}{
    The maximum number of iterations allowed.
  }
  \item{nstart}{
    If centers is a number, this parameters specifies how many random sets
    should be chosen.
  }
  \item{algorithm}{
    The algorithm to compute the kmeans. Currently disabled (default:
    ``\code{Lloyd}'') and kept for the future implementations.
  }
  \item{key}{
    Name of the column (from the table that is pointed by \code{x}) that
    contains the ids for each point.
  }
  \item{fn.dist}{
    The distance function used by MADlib to compute the objective function.
  }
  \item{agg.centroid}{
    The aggregate function used by MADlib to compute the objective function.
  }
  \item{min.frac}{
    The minimum fraction of centroids reassigned to continue iterating.
  }
  \item{kmeanspp}{
    Whether to call MADlib's kmeans++ centroid seeding method.
  }
  \item{seeding.sample.ratio}{
    The proportion of subsample of original dataset to use for kmeans++
    centroid seeding method.
  }
  \item{\dots}{
    Further arguments passed to or from other methods. Currently, no more
    parameters can be passed to madlib.kmeans.
  }
}

\details{

  See \code{\link{madlib.kmeans}} for more details.

}

\value{
  For the return value of kmeans clustering see \code{\link{madlib.kmeans}}
  for details.

  MADlib kmeans clustering output is similar to that of the kmeans output of
  the kmeans function of R package \code{stats}. \code{madlib.kmeans} also
  returns an object of class \code{"kmeans"} which has a \code{print} and a
  \code{fitted} method.It is a list with at least the following components:
  \item{cluster}{A vector of integers (from \code{1:k}) indicating the cluster
    to which each point is allocated.
  }
  \item{centers}{A matrix of cluster centres.}
  \item{withinss}{Vector of within-cluster sum of squares,
    one component per cluster.}
  \item{tot.withinss}{Total within-cluster sum of squares,
    i.e.\sspace{}\code{sum(withinss)}.}
  \item{size}{The number of points in each cluster.}
  \item{iter}{The number of (outer) iterations.}
}

\author{

    Author: Predictive Analytics Team at Pivotal Inc.

    Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}

}

\references{

[1] Documentation of kmeans clustering in the latest MADlib release,
    \url{http://madlib.incubator.apache.org/docs/latest/group__grp__kmeans.html}
}

\seealso{

    \code{\link{madlib.lm}}, \code{\link{madlib.summary}},
    \code{\link{madlib.arima}} are MADlib wrapper functions.

}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

dat <- db.data.frame("__madlib_km_sample__", conn.id = cid, verbose = FALSE)
cent <- db.data.frame("__madlib_km_centroids__", conn.id = cid, verbose = FALSE)

seed.matrix <- matrix(
  c(14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065,
    13.2,1.78,2.14,11.2,1,2.65,2.76,0.26,1.28,4.38,1.05,3.49,1050),
  byrow=T, nrow=2)

fit <- madlib.kmeans(dat, 2, key= 'key')
fit

## kmeans++ seeding method
fit <- madlib.kmeans(dat, 2, key= 'key', kmeanspp=TRUE)
fit # display the result

## Initial centroid table
fit <- madlib.kmeans(dat, centers= cent, key= 'key')
fit

## Initial centroid matrix
fit <- madlib.kmeans(dat, centers= seed.matrix, key= 'key')
fit

db.disconnect(cid)
}
}

\keyword{madlib}
