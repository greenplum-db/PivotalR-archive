\name{Compare-methods}

\alias{Compare methods}
\alias{>,character,db.obj-method}
\alias{>,db.obj,character-method}
\alias{>,db.obj,db.obj-method}
\alias{>,db.obj,numeric-method}
\alias{>,numeric,db.obj-method}
\alias{<,character,db.obj-method}
\alias{<,db.obj,character-method}
\alias{<,db.obj,db.obj-method}
\alias{<,db.obj,numeric-method}
\alias{<,numeric,db.obj-method}
\alias{>=,character,db.obj-method}
\alias{>=,db.obj,character-method}
\alias{>=,db.obj,db.obj-method}
\alias{>=,db.obj,numeric-method}
\alias{>=,numeric,db.obj-method}
\alias{<=,character,db.obj-method}
\alias{<=,db.obj,character-method}
\alias{<=,db.obj,db.obj-method}
\alias{<=,db.obj,numeric-method}
\alias{<=,numeric,db.obj-method}
\alias{==,character,db.obj-method}
\alias{==,db.obj,character-method}
\alias{==,db.obj,db.obj-method}
\alias{==,db.obj,numeric-method}
\alias{==,numeric,db.obj-method}
\alias{!=,character,db.obj-method}
\alias{!=,db.obj,character-method}
\alias{!=,db.obj,db.obj-method}
\alias{!=,db.obj,numeric-method}
\alias{!=,numeric,db.obj-method}
\alias{==,db.obj,logical-method}
\alias{==,logical,db.obj-method}
\alias{!=,db.obj,logical-method}
\alias{!=,logical,db.obj-method}
\alias{grepl}
\alias{grepl,character,db.obj-method}

\title{Comparison Operators for \code{\linkS4class{db.obj}} objects}

\description{
  These binary operators perform comparison on
  \code{\linkS4class{db.obj}} objects
}

\usage{
\S4method{>}{db.obj,db.obj}(e1, e2)
\S4method{<}{db.obj,db.obj}(e1, e2)
\S4method{>=}{db.obj,db.obj}(e1, e2)
\S4method{<=}{db.obj,db.obj}(e1, e2)
\S4method{==}{db.obj,db.obj}(e1, e2)
\S4method{!=}{db.obj,db.obj}(e1, e2)
\S4method{>}{character,db.obj}(e1, e2)
\S4method{<}{character,db.obj}(e1, e2)
\S4method{>=}{character,db.obj}(e1, e2)
\S4method{<=}{character,db.obj}(e1, e2)
\S4method{==}{character,db.obj}(e1, e2)
\S4method{!=}{character,db.obj}(e1, e2)
\S4method{>}{db.obj,character}(e1, e2)
\S4method{<}{db.obj,character}(e1, e2)
\S4method{>=}{db.obj,character}(e1, e2)
\S4method{<=}{db.obj,character}(e1, e2)
\S4method{==}{db.obj,character}(e1, e2)
\S4method{!=}{db.obj,character}(e1, e2)
\S4method{>}{numeric,db.obj}(e1, e2)
\S4method{<}{numeric,db.obj}(e1, e2)
\S4method{>=}{numeric,db.obj}(e1, e2)
\S4method{<=}{numeric,db.obj}(e1, e2)
\S4method{==}{numeric,db.obj}(e1, e2)
\S4method{!=}{numeric,db.obj}(e1, e2)
\S4method{>}{db.obj,numeric}(e1, e2)
\S4method{<}{db.obj,numeric}(e1, e2)
\S4method{>=}{db.obj,numeric}(e1, e2)
\S4method{<=}{db.obj,numeric}(e1, e2)
\S4method{==}{db.obj,numeric}(e1, e2)
\S4method{!=}{db.obj,numeric}(e1, e2)
\S4method{==}{db.obj,logical}(e1, e2)
\S4method{==}{logical,db.obj}(e1, e2)
\S4method{!=}{db.obj,logical}(e1, e2)
\S4method{!=}{logical,db.obj}(e1, e2)
\S4method{grepl}{character,db.obj}(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
}

\arguments{
  \item{e1, e2}{
    numeric, character or \code{\linkS4class{db.obj}} object.
  }

\item{pattern}{
character string containing a regular expression (or
          character string for 'fixed = TRUE') to be matched in the
          given character vector.
}

\item{x}{
A \code{\linkS4class{db.obj}} object.
}

\item{ignore.case}{
if 'FALSE', the pattern matching is _case sensitive_ and
          if 'TRUE', case is ignored during matching.
}

\item{perl}{
logical.  Should perl-compatible regexps be used? Not implemented yet.
}

\item{fixed}{
logical.  If 'TRUE', 'pattern' is a string to be matched as
          is.  Overrides all conflicting arguments.
}

\item{useBytes}{
logical. Not implemented yet.
}
}

\value{
  \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  that computes the comparison operations.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  A meaningful expression is generated only when the
  \code{.col.data_type} is "character" or "numeric", otherwise a "NULL"
  value is
  generated.
}

\seealso{
\code{\linkS4class{db.Rquery}} contains a SQL query that does the
operations.
}

\examples{
\dontrun{
## get the help for a method
## help(">,db.obj,db.obj-method")
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)

lk(x[x$length > 10,])

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{math}
