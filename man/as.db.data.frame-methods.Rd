\name{as.db.data.frame}
\alias{as.db.data.frame}
\alias{as.db.data.frame,character-method}
\alias{as.db.data.frame,data.frame-method}
\alias{as.db.data.frame,db.Rquery-method}

\title{
  Convert other objects into a \code{db.data.frame} object
}

\description{
  Methods for function \code{as.db.data.frame} in package \pkg{PivotalR} ~~
}

\usage{
\S4method{as.db.data.frame}{data.frame}(x, table.name, verbose = FALSE,
conn.id = 1, add.row.names = FALSE, key = character(0), distributed.by =
NULL, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{character}(x, table.name, verbose = FALSE,
conn.id = 1, add.row.names = FALSE, key = character(0), distributed.by =
NULL, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{db.Rquery}(x, table.name, verbose = FALSE,
is.view = FALSE, is.temp = FALSE, pivot = TRUE, distributed.by = NULL)
}

\arguments{



}

\section{Methods}{
\describe{

\item{\code{signature(x = "character")}}{
describe this method here~~
}

\item{\code{signature(x = "data.frame")}}{
Describe this method here~~
}

\item{\code{signature(x = "db.Rquery")}}{
Describe this method here~~
}

}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
