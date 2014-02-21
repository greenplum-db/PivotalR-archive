\name{db.Rquery-class}
\Rdversion{1.1}
\docType{class}
\alias{db.Rquery-class}
\alias{db.Rview-class}

\title{Class \code{"db.Rquery"} and its sub-class \code{db.Rview-class}}
\description{
  An object of this class represents a series of operations applied onto
  an existing \code{\linkS4class{db.data.frame}} object. These
  operations are actually a SQL query, which one can choose to
  materialize in the database using
  \code{\link{as.db.data.frame}}. \code{\link{lk}} can fetch a part
  of the result of executing the SQL query. Thus one does not need to
  create a table for every step of the operations, and the data
  transfered between R and the database is minimized.
}

\section{Objects from the Class}{
Objects can be created by almost all functions/methods that can be
applied onto \code{\linkS4class{db.data.frame}} except
\code{\link{content}}, \code{\link{lk}} and \code{\link{delete}}.

\code{db.Rview-class} is a sub-class of \code{db.Rquery-class}, and it behaves just like "view" in the databases except that it exists only in R. Usually there is no difference to use \code{db.Rview} or \code{db.Rquery}. \code{\link{as.db.Rview}} casts a \code{db.Rquery} object into a \code{db.Rview} object.

Usually it is
NOT recommended to directly manipulate the internal slots of these objects.
}

\section{Slots}{
  \describe{
    \item{\code{.content}:}{
      Object of class \code{"character"}. The SQL query that represents
  the operations. The function \code{\link{content}} can get this value.
    }

    \item{\code{.expr}:}{
      Object of class \code{"character"}. An array of expression
      strings for columns of the table that the SQL query can be
      materialized into. It is not to used by the
      normal users.
    }

    \item{\code{.source}:}{
      Object of class \code{"character"}. A string, the table/view name
      which this SQL query is originated. It is not to used by the
      normal users.
    }

    \item{\code{.parent}:}{
      Object of class \code{"character"}. A string. In the SQl query it
      is the part after "from". It is not to used by the
      normal users.
    }

    \item{\code{.conn.id}:}{
      Object of class \code{"numeric"}, an integer. The ID number of the
      database
      connection where \code{.source} resides. The functions
      \code{\link{conn.id}} and \code{\link{conn.id<-}} can get and set
      this value.
    }

    \item{\code{.col.name}:}{
      Object of class \code{"character"}. An array of strings. The names
      of columns of the table that the SQL query can be materialized
      into. The
      S4 method \code{\link{names,db.obj-method}} gets this value.
    }

    \item{\code{.key}:}{
      Object of class \code{"character"}. The name of the primary key
      column name in \code{.source}. Currently only one primary key
      column is
      supported. This value can be set during the creation of the object
      when using the function \code{\link{db.data.frame}}. The functions
      \code{\link{key}} and \code{\link{key<-}} can be used to get and
      set this value.
    }

    \item{\code{.col.data_type}:}{
      Object of class \code{"character"}.  The 1D array of column data
      types of the table that the SQL query can be materialized
      into. This is not supposed to be used by the normal user.
    }

    \item{\code{.col.udt_name}:}{
      Object of class \code{"character"}. The 1D array of column udt
      names of the table that the SQL query can be materialized
      into. This is
      not to used by normal users.
    }

    \item{\code{.where}:}{
      Object of class \code{"character"}. The condition string used in
      "where" inside the SQL query.
    }

    \item{\code{.is.factor}:}{
      Object of class \code{"logical"}. An array of logical values which
      indicate whether each column of the table that the SQL query can
      be materialized
      into is a factor. This
      is not to be used by the normal users.
    }

    \item{\code{.factor.suffix}:}{
      Object of class \code{"character"}. An array of strings for
      every column. When creating dummy columns
      for a factor column, we add a random string in the names of the
      dummy columns to avoid naming conflicts. So a factor column's
      \code{.factor.suffix} is a random string, otherwise it is just an
      empty string. This is not to be used by the normal users. It is
      used only the MADlib wrapper functions that support categorical
      variables.
    }

    \item{\code{.factor.ref}:}{
      The value of the factor reference level for the regressions. If it       is \code{NA}, then the regressions automatically select a reference level.
    }

    \item{\code{.sort}:}{
      Object of class \code{"list"}. The list contains the information
      used for "order by" in the SQL query.

      by: A string. The column names that are used in "order by".

      order: A string, "" or "desc"

      str: A string, the full "order by ..." string.
    }

    \item{\code{.is.agg}:}{
      logical value, whether this object represents an aggregate
      operation.
    }

      \item{\code{.dist.by}:}{
    A string, the distribution policy for the original data table, which is used to construct this \code{db.Rquery} object, when using Greenplum database or HAWQ. It can be \code{character(0)}, which means the original data table is distributed randomly. Or it can be a string of column names separated by comma, which are the columns that are used in the "distributed by" when the original table was created.
  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{db.obj}"}, directly.
}

\section{Methods}{
All methods for \code{\linkS4class{db.data.frame}} can be applied onto
this class.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.data.frame}} creates a \code{db.data.frame} object.

  \code{\link{as.db.data.frame}} converts \code{db.Rquery} object,
  \code{data.frame}, or a data file into a \code{db.data.frame} object
  and at the same time creates a new table in the database.

  \code{\link{as.db.Rview}} converts a \code{db.Rquery} object to a \code{db.Rview} object.

  \code{\linkS4class{db.obj}} is the superclass.

  Class \code{\linkS4class{db.data.frame}} is another sub-class of
  \code{\linkS4class{db.obj}}.

  \code{\link{lk}} display a part of the table
}

\examples{
\dontrun{
showClass("db.Rquery")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname)

delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid)

## create several db.Rquery objects
y <- x[,1:2]
z <- x[x$rings > 10,]

dim(z) # get an error

lk(y)

lk(z)

## materialize a db.Rquery object
z <- as.db.data.frame(z, "abalone_rings_larger_10")
delete("abalone_rings_larger_10", conn.id = cid)

dim(z) # no error

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{classes}
\keyword{database}