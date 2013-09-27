\name{as.environment}

\alias{as.environment.db.table}
\alias{as.environment.db.view}

\title{Convert a database table or view into an environment}

\description{
    These functions allow a \code{\link{db.table}} or \code{\link{db.view}} object to be treated as an environment, in a manner analogous to data frames.
}

\usage{
    method{as.environment}{db.table}(x, ...)
    method{as.environment}{db.view}(x, ...)
}

\arguments{
    \item{x}{
        A \code{db.table} or \code{db.view} object to treat as an environment.
    }
    \item{...}{
        Other arguments; unused.
    }
}

\value{
    The created environment. Note that no data is transferred to the client; all objects in the environment are queries pointing back to the host.
}

\seealso{
    \code{\link{as.environment}}
}


