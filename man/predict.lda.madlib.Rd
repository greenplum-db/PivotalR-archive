\name{predict.lda}
\alias{predict.lda.madlib}

\title{Prediction function for MADlib's LDA models}

\description{Labelling test documents using a learned LDA model
built by \code{linlk{madlib.lda}}}

\usage{
\method{predict}{lda.madlib}(object, data, docid, words, ...)
}

\arguments{
  \item{object}{
    The result of \code{\link{madlib.lda}}.
  }

  \item{data}{
    An object of \code{db.obj} class. This is the database table
    containing the documents on which the algorithm will predict.
    The text of each document should be tokenized into 'words'.
  }

  \item{docid}{
    Text name of the column containing the id of the documents.
  }

  \item{words}{
    Column name of the input data table containing the vector of words/tokens in the documents.
  }

  \item{\dots}{
    Arguments passed to or from other methods, not implemented yet.
  }
}

\value{
  A \code{\linkS4class{db.table}} object, which points to a table that
  contains the predicted values. The table has the following columns:
    docid
    wordcount
    words
    counts
    topic_count
    topic_assignment

  One can use the function \code{\link{lk}} to look at the values.
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
