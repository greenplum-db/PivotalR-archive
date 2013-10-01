\name{as.environment}
\alias{as.environment}
\alias{with}
\alias{as.environment.db.table}
\alias{as.environment.db.view}
\alias{with.db.table} \alias{with.db.view}

\title{Evaluate expressions within the context of a database table or view}

\description{ These functions allow a \code{\linkS4class{db.table}} or
    \code{\linkS4class{db.view}} object to be treated as an environment,
    in a manner analogous to data frames. }

\usage{
    \method{as.environment}{db.table}(x, ...)
    \method{as.environment}{db.view}(x, ...)
    \method{with}{db.table}(data, expr, ...)
    \method{with}{db.view}(data, expr, ...)
}

\arguments{
    \item{x, data}{
        A \code{db.table} or \code{db.view} object to treat as an environment.
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
    
    For \code{with}, a \code{\linkS4class{db.Rquery}} stored query object representing the expression. Use \code{\link{preview}} or \code{\link{as.data.frame}} to execute the query on the host and retrieve its contents.
  }

  \author{ Author: Hong Ooi, Pivotal Inc. \email{hooi@gopivotal.com}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{as.environment}}
}


