\name{merge-method}

\alias{merge,db.obj,db.obj-method}

\title{Computing a join on two tables}

\description{
This method is equivalent to an SQL \it{join} on two tables, and the merge can be by common columns or row names.  It supports the equivalent of inner, left-outer, right-outer, and full-outer join operations.  This method is similar to \link{merge.data.frame}.  
}

\usage{
\S4method{merge}{db.obj,db.obj}(x, y, ...)

\S4method{merge}{db.obj,db.obj}(x, y, by = intersect(names(x), names(y)),
by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
key = x@.key, suffixes = c("_x","_y"), ...)
}

\arguments{
  \item{x,y}{
    The signature of the method.  Both argument are \code{db.obj} objects, and their associated tables will be joined.  
  }

  \item{by,by.x,by.y}{
    specifications of the columns used for merging. See ‘Details’.
  }

  \item{all}{
    logical; \code{all = L} is shorthand for \code{all.x = L} and \code{all.y = L}, where \code{L} is either \code{TRUE} or \code{FALSE}.
  }

  \item{all.x}{
    logical; if \code{TRUE}, then extra rows will be added to the output, one for each row in \code{x} that has no matching row in \code{y}. These rows will have \code{NA}s in those columns that are usually filled with values from \code{y}. The default is \code{FALSE}, so that only rows with data from both \code{x} and \code{y} are included in the output.
  }

  \item{all.y}{
    logical; analogous to \code{all.x}.  
  }

  \item{key}{
    specifies the primary key the of the newly created table.   
  }

  \item{suffixes}{
    a character vector of length 2 specifying the suffixes to be used for making unique the names of columns in the result which not used for merging (appearing in \code{by} etc).
  }

  \item{\dots}{
    arguments to be passed to or from methods.
  }
}

\details{
See \link{merge.data.frame}.  Note that \code{merge.data.frame} supports a \code{incomparables} argument, which is not yet supported here.  
}

\value{
  A \code{\linkS4class{db.Rquery}} object, which expresses the \it{join} operation.  
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{merge.data.frame}} a merge operation for two data frames.  
}

\examples{
\dontrun{
## some examples
}
}

\keyword{methods}
\keyword{database}
