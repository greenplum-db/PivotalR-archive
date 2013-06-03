\name{merge-method}

\alias{merge,db.obj,db.obj-method}

\title{ ~~ Methods for Function \code{merge} in Package \pkg{base} ~~}

\description{
 ~~ Methods for function \code{merge} in package \pkg{base} ~~
}

\usage{
\S4method{merge}{db.obj,db.obj}(x, y, by = intersect(names(x), names(y)),
by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
key = x@.key, suffixes = c("_x","_y"), ...)
}

\arguments{
  \item{x,y}{
    To add
  }

  \item{by}{
    to add
  }

  \item{by.x}{
    to add
  }

  \item{by.y}{
    to add
  }

  \item{all}{
    to add
  }

  \item{all.x}{
    to add
  }

  \item{all.y}{
    to add
  }

  \item{key}{
    to add
  }

  \item{suffixes}{
    to add
  }

  \item{\dots}{
    to add
  }
}

\value{
  A \code{\linkS4class{db.Rquery}} object. It contains the SQL query
  which ...
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{arraydb.to.arrayr}} convert strings extracted form
  database into arrays.
}

\examples{
\dontrun{
## som examples
}
}

\keyword{methods}
\keyword{database}
