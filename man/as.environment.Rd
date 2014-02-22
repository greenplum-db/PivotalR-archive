\name{as.environment}
\alias{as.environment}
\alias{with}
\alias{as.environment.db.obj}
\alias{with.db.obj}

\title{Evaluate expressions within the context of a database table or view}

\description{ These functions allow a \code{\linkS4class{db.table}} or
    \code{\linkS4class{db.view}} object to be treated as an environment,
    in a manner analogous to data frames. }

\usage{
    \method{as.environment}{db.obj}(x, ...)
    \method{with}{db.obj}(data, expr, ...)
}

\arguments{
    \item{x, data}{
        A \code{\linkS4class{db.obj}} object to treat as an environment.
    }
    \item{expr}{
        For \code{with}, an R expression to evaluate in the context of a database table or view.
    }
    \item{...}{
        Other arguments; unused.
    }
}

\value{
  For \code{as.environment}, the created environment. Note that no data is transferred to the client; all objects in the environment are queries pointing back to the host.

For \code{with}, a \code{\linkS4class{db.Rquery}} stored query object
representing the expression. Use \code{\link{lk}}, \code{\link{lookat}} or \code{\link{as.data.frame}} to execute the query on the host and retrieve its contents.
}

\author{
  Author: Hong Ooi, Pivotal Inc. \email{hooi@gopivotal.com}

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{as.environment}}
}