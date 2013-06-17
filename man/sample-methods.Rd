\name{sample-methods}

\alias{sample,db.obj-method}

\title{ Methods for Function \code{sample} in Package \pkg{base} ~~}

\description{
 Methods for function \code{sample} in package \pkg{base} ~~
}

\usage{
\S4method{sample}{db.obj}(x, size, replace = FALSE, prob = NULL, ...)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object.
  }

  \item{size}{
    An integer.
  }

  \item{replace}{
    A logical value, default is \code{FALSE}.
  }

  \item{prob}{
    A vector of double values, default is \code{NULL}. The
    probabilityies of each row to sample. Not implemented yet.
  }

  \item{\dots}{
    Extra parameters
  }
}

\details{
  Some details
}

\value{
  A \code{\linkS4class{db.data.frame}} object.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  see also
}

\examples{
\dontrun{
## Example 1
}
}

\keyword{methods}
\keyword{database}
\keyword{utility}
