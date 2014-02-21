\name{db.data.frame-class}
\Rdversion{1.1}
\docType{class}
\alias{db.data.frame-class}

\title{Class \code{"db.data.frame"}}
\description{
  An object of this class points to a real table/view in the
  database. No data is transfered into R. Only a minimal amount of
  information is kept in the object.
}

\section{Objects from the Class}{
Objects can be created by calls of \code{\link{db.data.frame}} or
\code{\link{as.db.data.frame}}.

The object represents a real table/view in the database. Usually it is
NOT recommended to directly manipulate the internal slots of these objects.
}

\section{Slots}{
  \describe{
    \item{\code{.name}:}{
      Object of class \code{"character"}. It is the table name if this
      \code{db.data.frame} was created using just a table name. It can
      also be a two-element array if this \code{db.data.frame} was
      created. This slot is obsolete.
    }

    \item{\code{.content}:}{
      Object of class \code{"character"}. The table
      name. The function \code{\link{content}} can get this value.
    }

    \item{\code{.conn.id}:}{
      Object of class \code{"numeric"}, an integer. The ID number of the
      database
      connection where the table resides. The functions
      \code{\link{conn.id}} and \code{\link{conn.id<-}} can get and set
      this value.
    }

    \item{\code{.col.name}:}{
      Object of class \code{"character"}. The 1D array of column names
      of the table/view that this \code{db.data.frame} points to. The
      S4 method \code{\link{names,db.obj-method}}} gets this value.

    \item{\code{.col.data_type}:}{
      Object of class \code{"character"}. The 1D array of column data
      types of the table/view that this \code{db.data.frame} points
      to. This is not supposed to be used by the normal user.}

    \item{\code{.col.udt_name}:}{
      Object of class \code{"character"}. The 1D array of column udt
      names of
      the table/view that this \code{db.data.frame} points to. This is
      not to used by normal users.
    }

    \item{\code{.table.type}:}{
      Object of class \code{"character"}. The information about the type
      of tha table/view that this \code{db.data.frame} points
      to, for example, "BASE TABLE", "VIEW" or "LOCAL TEMPORARY".
    }

    \item{\code{.is.factor}:}{
      Object of class \code{"logical"}. An array of logical values which
      indicate whether each column of the table/view is a factor. This
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
      The value of the factor reference level for the regressions. If it is \code{NA}, then the regressions automatically select a reference level.
    }

    \item{\code{.appear.name}:}{Object of class \code{"character"}. This
  is also related the factor columns. \code{\link{print.lm.madlib}} and
  \code{\link{print.logregr.madlib}} use this value for printing the
  names of the dummy columns. This is not to be used by the normal users.}

  \item{\code{.dummy}:}{
    Object of class \code{"character"}. An array of strings,
    The dummy column names which are
    used only for factor support.
  }

  \item{\code{.dummy.expr}:}{
    Object of class \code{"character"}. The SQL expressions used to
    create dummy column names which are
    used only for factor support.
  }

  \item{\code{.dist.by}:}{
    A string, the distribution policy when using Greenplum database or HAWQ. It can be \code{character(0)}, which means the data table is distributed randomly. Or it can be a string of column names separated by comma, which are the columns that are used in the "distributed by" when the table was created.
  }
}
}

\section{Extends}{
Class \code{\linkS4class{db.obj}}, directly.
}

\section{Methods}{
\code{\link{Aggregate functions}}, \code{\link{by,db.obj-method}},
\code{\link{dim,db.table-method}}, \code{\link{dim,db.view-method}},
\code{\link{dim,db.Rquery-method}}, \code{\link{names,db.obj-method}},
\code{\link{conn.id}}, \code{\link{conn.id<-}},
\code{\link{eql}}, \code{\link{key}}, \code{\link{key<-}},
\code{\link{merge,db.obj,db.obj-method}},
\code{\link{print,db.data.frame-method}},
\code{\link{show,db.data.frame-method}},
\code{\link{sort,db.obj-method}}, \code{\link{subset,db.obj-method}},
\code{\link{Arith methods}}, \code{\link{Compare methods}},
\code{\link{Logical methods}}, \code{\link{Extraction methods}},
\code{\link{Replacement methods}}, \code{\link{madlib.lm}},
\code{\link{madlib.glm}}, \code{\link{madlib.summary}}
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

  \code{\linkS4class{db.obj}} is the superclass.

  \code{\linkS4class{db.table}} and \code{\linkS4class{db.view}} are the
  sub-classes.

  \code{\linkS4class{db.Rquery}} is another sub-class of
  \code{\linkS4class{db.obj}}.

  \code{\link{lk}} or \code{\link{lookat}} display a part of the table
}

\examples{
\dontrun{
showClass("db.data.frame")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", conn.id = cid)
as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)
x <- db.data.frame("abalone", conn.id = cid, verbose = FALSE) # x points to table "abalone"

lk(x)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{classes}
\keyword{database}