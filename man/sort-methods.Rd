\name{sort}

\alias{sort,db.obj-method}

\title{ ~~ Methods for Function \code{sort} in Package \pkg{base} ~~} 

\description{
 ~~ Methods for function \code{sort} in package \pkg{base} ~~
}

\usage{
\S4method{sort}{db.obj}(x, decreasing = FALSE, INDICES, ...)
}

\arguments{
  \item{x}{
    object
  }

  \item{decreasing}{
    desc
  }

  \item{INDICES}{
    A list of \code{db.Rquery} objects. Each of
    the list element selects one or multiple columns of \code{x}.
    \code{NULL} to order by random(). 
  }

  \item{...}{
    What's this? Seems not implemented.
  }
}

\value{
  \code{db.Rquery}
}

\author{
  Pivotal
}

\seealso{
  \code{\link{preview}}

  \code{\link{db.Rquery-class}}
}

\examples{
## example
}

\keyword{methods}
\keyword{operator}
