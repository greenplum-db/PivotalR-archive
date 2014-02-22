\name{arraydb.to.arrayr}

\alias{arraydb.to.arrayr}

\title{
  Convert strings extracted from database into arrays
}

\description{
  An array object in database is converted to a string when passed into
  R, for example \code{'\{1.2, 3.4, 5.7\}'}, and this function can convert the
  string to an array in R, for example \code{c(1.2, 3.4, 5.7)}. This function
  can also convert a vector of such strings into a two-dimensional
  array.
}

\usage{
arraydb.to.arrayr(str, type = "double", n = 1)
}

\arguments{
  \item{str}{
    A vector of strings, or a single string, that has multiple elements
  in it and deliited by \code{","}.
  }
  \item{type}{
    The type of the return value of this function. Default is
  \code{"double"}. It can be \code{"character"}, \code{"double"},
  \code{"logical"}, \code{"integer"},
  \code{"numeric"} etc. All types other than \code{"character"},
  \code{"logical"} and
  \code{"integer"} will be treated as \code{"numeric"}.
  }
  \item{n}{
    An integer, default is 1. If the input has \code{NA} instead of a string as
   one element of a string array, how many \code{NA}'s should be returned so that
    a valid array can be returned. There should be as many \code{NA} as the number
  of elements in other output rows without \code{NA}.
  }
}

\details{
  When R reads in data from a table in the database, the result is a
  data.frame object. However, if the orginal data table has a column
  which is the array type, the array is automatically converted into a
  string and data.frame object has a corresponding column of strings,
  each of which starts with \code{"\{"} and ends with \code{"\}"},
  and all the original
  array elements are casted into strings delimited by ",".

  For example, the array in database \code{array['ab', 'c d', '"axx, t"']}
  becomes a string in R \code{'{ab, c d, \"axx, t\"}'}.

  This function deals with such strings and turn them into faimiliar
  arrays that users can directly use.
}

\value{
  A two dimensional array, whose element's type is decided by the
  function argument \code{type}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  (1) The returned value is a two dimensional array, even if \code{str}
  is a single string.

  (2) Although this function is for the strings extracted from database,
  it can actually deal with strings like \code{"a, b, c"}, which do not start
  or end with curly brackets.
}

\seealso{
  \code{\link{lk}} or \code{link{lookat}} extracts the data of a table
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## Example 1 ----------

str <- '{1.2, 3.4, 5.6}'
arraydb.to.arrayr(str, "double") # c(1.2, 3.4, 5.6)

str <- '{a, b, "c, d"}'
arraydb.to.arrayr(str, "character") # c("a", "b", "\"c, d\"")

## Example 2 ----------

## table_in_database has a column of arrays
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
x$col.array <- db.array(x[,3:10])
dat <- lk(x, nrows = 50, array = FALSE) # extract the actual data
arraydb.to.arrayr(dat$col.array, "double") # an array of 50 rows

## ----------------------------------------------------------------------
db.disconnect(cid, verbose = FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ database }
\keyword{ utility }
