\name{sample-methods}

\alias{sample,db.obj-method}

\title{Methods for sampling rows of data from a table/view randomly}

\description{
  This method samples rows of data from a table/view randomly. The
  sampled result is stored in a temporary table.
}

\usage{
\S4method{sample}{db.obj}(x, size, replace = FALSE, prob = NULL, ...)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object, which is the wrapper to the
  data table.
  }

  \item{size}{
    An integer. The size of the random sample. When \code{replace} is
    \code{FALSE}, \code{size} must be smaller than the data table/view's
    total row number.
  }

  \item{replace}{
    A logical value, default is \code{FALSE}. When it is \code{TRUE},
    the data is sampled with replacement, which means a row might be
    sampled for multiple times. When it is \code{FALSE}, each row can
    only be sampled at most once.
  }

  \item{prob}{
    A vector of double values, default is \code{NULL}. The
    probabilityies of each row to sample. Not implemented yet.
  }

  \item{\dots}{
    Extra parameters. Not implemented.
  }
}

\details{
  When \code{replace} is \code{FALSE}, the data is just sorted randomly
  (see \code{\link{sort,db.obj-method}}) and selected, which is similar
to \code{sort(x, FALSE, "random")}. When
  \code{replace} is \code{TRUE}, we have to scan the table multiple
  times to select repeated items.
}

\value{
  A \code{\linkS4class{db.data.frame}} object, which is a wrapper to a
  temporary table. The table contains the sampled data.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{generic.bagging}} uses \code{sample}
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

y <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(y, 10)

dim(y)

a <- sample(y, 20)

dim(a)

lookat(a)

b <- sample(y, 40, replace = TRUE)

dim(b)

lookat(b)

delete(b)

db.disconnect(cid, verbose = FALSE)
}
}
\keyword{methods}
\keyword{database}
\keyword{stats}
\keyword{math}
\keyword{utility}
