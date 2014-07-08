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

plr.reduce(fun, data, ...)

plr(FUN, conn.id = 1)

plr.agg(FUN, conn.id = 1)

declare(...)
}
