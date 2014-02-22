\name{scale}
\alias{scale,db.obj-method}
\title{Scaling and centering of tables }
\description{
  \code{scale} centers and/or scales the columns of a numeric table.
}

\usage{
\S4method{scale}{db.obj}(x, center = TRUE, scale = TRUE)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object. It represents a table/view in the database
  if it is an \code{\linkS4class{db.data.frame}} object,
  or a series of operations applied on an existing \code{\linkS4class{db.data.frame}}
  object if it is a \code{\linkS4class{db.Rquery}} object.
  }

  \item{center}{
    either a logical value or a numeric vector of length equal to
    the number of columns of 'x'.
  }

  \item{scale}{
    either a logical value or a numeric vector of length equal to
    the number of columns of 'x'.
  }
}

\details{
   The value of 'center' determines how column centering is
     performed.  If 'center' is a numeric vector with length equal to
     the number of columns of 'x', then each column of 'x' has the
     corresponding value from 'center' subtracted from it.  If 'center'
     is 'TRUE' then centering is done by subtracting the column means
     (omitting 'NA's) of 'x' from their corresponding columns, and if
     'center' is 'FALSE', no centering is done.

     The value of 'scale' determines how column scaling is performed
     (after centering).  If 'scale' is a numeric vector with length
     equal to the number of columns of 'x', then each column of 'x' is
     divided by the corresponding value from 'scale'.  If 'scale' is
     'TRUE' then scaling is done by dividing the (centered) columns of
     'x' by their standard deviations if 'center' is 'TRUE', and the
     root mean square otherwise.  If 'scale' is 'FALSE', no scaling is
     done.

   The root-mean-square for a (possibly centered) column is defined
     as sqrt(sum(x^2)/(n-1)), where x is a vector of the non-missing
     values and n is the number of non-missing values.  In the case
     'center = TRUE', this is the same as the standard deviation, but
     in general it is not.  (To scale by the standard deviations
     without centering, use 'scale(x, center = FALSE, scale = lookat(sd(x)))'.)
}

\value{
  A \code{db.Rquery} object. It computes the centering and/or scaling of
  code{x} for each column including array elements. The result can be
  viewed using \code{\link{lk}} or \code{\link{lookat}}.

  The numeric
  centering and scalings used (if any) are returned as attributes
  "scaled:center" and "scaled:scale". The number of rows in the table is also
  returned as the attribute "row.number".
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.array}} creates an array column for a
  \code{\linkS4class{db.Rquery}} object.
}

\examples{
\dontrun{
## help("scale,db.obj-method") # display this doc
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname)

x <- as.db.data.frame(abalone, conn.id = cid)
lk(x, 10)

s <- scale(x[-c(1,2)]) # scale all numeric columns

centers <- attr(s, "scaled:center")
scales <- attr(s, "scaled:scale")

## create the scaled table
delete("scaled_abalone")
y <- as.db.data.frame(s, "scaled_abalone")

lk(y, 10)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{math}
\keyword{stats}