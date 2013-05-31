\name{sort}

\alias{sort,db.obj-method}

\title{ ~~ Methods for Function \code{sort} in Package \pkg{base} ~~} 

\description{
 ~~ Methods for function \code{sort} in package \pkg{base} ~~
}

\usage{
\S4method{sort}{db.obj}(x, decreasing = FALSE, by = character(0), ...)
}

\arguments{
  \item{x}{
    object
  }

  \item{decreasing}{
    desc
  }

  \item{by}{
    A string. \code{NULL} to order by random(). \code{character(0)} to
    order by 
    the key, no key then raise an error. 
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
