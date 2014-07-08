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
    \item{fun}
}
