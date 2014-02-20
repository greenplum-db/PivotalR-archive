\name{Extract-Replace-methods}

\alias{Extraction methods}
\alias{Replacement methods}
\alias{$,db.obj-method}
\alias{$<-,db.obj,character-method}
\alias{$<-,db.obj,integer-method}
\alias{$<-,db.obj,numeric-method}
\alias{$<-,db.obj,db.Rquery-method}
\alias{$<-,db.obj,logical-method}
\alias{[,db.obj,ANY,ANY,ANY-method}
%% \alias{[<-,db.obj,ANY,ANY,character-method}
%% \alias{[<-,db.obj,ANY,ANY,integer-method}
%% \alias{[<-,db.obj,ANY,ANY,numeric-method}
%% \alias{[<-,db.obj,ANY,ANY,db.Rquery-method}
%% \alias{[<-,db.obj,ANY,ANY,logical-method}
\alias{[<-.db.obj}
\alias{[[,db.obj-method}
\alias{[[<-,db.obj,ANY,ANY,character-method}
\alias{[[<-,db.obj,ANY,ANY,integer-method}
\alias{[[<-,db.obj,ANY,ANY,numeric-method}
\alias{[[<-,db.obj,ANY,ANY,db.Rquery-method}
\alias{[[<-,db.obj,ANY,ANY,logical-method}

\title{Extract or replace a part of \code{\linkS4class{db.obj}} objects}

\description{
  Operators acting on \code{\linkS4class{db.obj}} objects to extract
  or replace parts.
}

\usage{
\S4method{$}{db.obj}(x, name)
\S4method{[[}{db.obj}(x, i, j, ...)
\S4method{[}{db.obj,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)
\S4method{$}{db.obj,character}(x, name) <- value
\S4method{$}{db.obj,integer}(x, name) <- value
\S4method{$}{db.obj,numeric}(x, name) <- value
\S4method{$}{db.obj,logical}(x, name) <- value
\S4method{$}{db.obj,db.Rquery}(x, name) <- value
\S4method{[[}{db.obj,ANY,ANY,character}(x, i, j) <- value
\S4method{[[}{db.obj,ANY,ANY,integer}(x, i, j) <- value
\S4method{[[}{db.obj,ANY,ANY,numeric}(x, i, j) <- value
\S4method{[[}{db.obj,ANY,ANY,logical}(x, i, j) <- value
\S4method{[[}{db.obj,ANY,ANY,db.Rquery}(x, i, j) <- value
\method{[}{db.obj}(x, i, j) <- value
%% x$name
%% x[[i, j, ...]]
%% x[i, j, ..., drop = TRUE]

%% x$name <- value
%% x[[i, j]] <- value
%% x[i, j] <- value
}

\arguments{
\item{x}{
       A \code{db.obj} (either \code{db.table}, \code{db.view},
        or \code{db.Rquery}) from which to extract element(s).
}

    \item{i, j, ...}{
        Indices specifying elements to extract or replace.  Indices
          are `numeric' or `character' vectors or
        \code{\linkS4class{db.Rquery}} object or empty (missing) or
          `NULL'.
    }

\item{name}{
A string. The column name.
}

\item{value}{
Any valid value, including \code{\linkS4class{db.Rquery}},
\code{character}, \code{numeric}, \code{integer}, and \code{logical}
object. The
value that is used to replace the part of the \code{db.obj}.
}

\item{drop}{
Not implemented yet.
}
}

\value{
    A \code{db.Rquery} object is returned. For the extraction methods,
    this is a
    SQL query to extract the requested subset. For the replacement
    methods, this is a SQL query representing the modified version of
    \code{x}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{subset,db.obj-method}} Operator to extract elements
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

x$area <- x[["length"]] * x[,"height"] # add a new column

y <- x[,-c(1,2)] # use all columns except the first two

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{data operation}
\keyword{utility}