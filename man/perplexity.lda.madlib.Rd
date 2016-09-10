\name{perplexity.lda}
\alias{perplexity.lda.madlib}

\title{Perplexity of LDA predictions}

\description{Perplexity is a measurement of how well a probability
distribution or probability model predicts a sample. This functions computes
the perplexity of the prediction by \code{linlk{predict.madlib.lda}}}

\usage{
\method{perplexity}{lda.madlib}(object, predict_output_table, ...)
}

\arguments{
  \item{object}{
    The result of \code{\link{madlib.lda}}.
  }

  \item{predict_output_table}{
    The result of \code{predict} on the \code{\link{madlib.lda}} object.
  }

  \item{\dots}{
    Arguments passed to or from other methods, not implemented yet.
  }
}

\value{
  A numeric value that indicates the perplexity of the LDA prediction.
}

\author{
 Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
  \code{\link{madlib.lda}} builds a topic model using a set of documents.
}

\examples{
\dontrun{
## Please see the examples in madlib.lda doc.
}
}

\keyword{madlib}
\keyword{stats}
\keyword{math}
