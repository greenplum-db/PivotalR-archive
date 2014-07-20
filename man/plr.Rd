\name{PL/R Support}
\alias{plr.map}
\alias{plr.reduce}
\alias{plr}
\alias{plr.agg}
\alias{decalre}

\title{
    Directly send R code into the database to execute using PL/R
}

\description{
    Using PL/R plugin of the PostgreSQL database, we can directly
    send R code into the database to execute. This adds great
    flexibility to what we can do with this package.
}

\usage{
plr.map(fun, data, ...)

plr.reduce(fun, data, init = NA, ...)

plr(fun, conn.id = 1)

plr.agg(fun, conn.id = 1, init = NA)

declare(...)
}

\arguments{
    \item{fun}{

    }

    \item{data}{

    }

    \item{init}{

    }

    \item{conn.id}{

    }

    \item{...}{

    }
}

\value{

}

\author{
    Author: Predictive Analytics Team at Pivotal Inc.

    Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{

}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)



db.disconnect(cid, verbose = FALSE)
}
}
