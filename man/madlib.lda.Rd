\name{madlib.lda}
\alias{madlib.lda}

\title{
  Wrapper for MADlib's Latent Dirichilet Allocation
}

\description{
  This function is a wrapper for MADlib's Latent Dirichlet Allocation. The
  computation is parallelized by MADlib if the connected database is distributed.
}

\usage{
madlib.lda(data, docid, words, topic_num, alpha, beta, iter_num = 20,...)
}

\arguments{

  \item{data}{
      An object of \code{db.obj} class. This is the database table
      containing the documents on which the algorithm will train.
      The text of each document should be tokenized into 'words'.
    }

  \item{docid}{
      Text name of the column containing the id of the documents.
  }

  \item{words}{
      Column name of the input data table containing the vector of words/tokens in the documents.
  }

  \item{topic_num}{
      Number of topics.
  }

  \item{alpha}{
      Dirichlet parameter for the per-doc topic multinomial.
  }

  \item{beta}{
       Dirichlet parameter for the per-topic word multinomial.
  }

  \item{iter_num}{
      Number of iterations.
  }

  \item{\dots}{
    Other optional parameters. Not implemented.
  }

}
