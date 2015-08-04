\name{madlib.lda}

\alias{madlib.lda}

\title{
  Latent Dirichilet Allocation
}

\description{
  The wrapper function for MADlib Latent Dirichlet Allocation. The
  computation is parallelized by MADlib if the connected database is
  Greenplum database. 
}

\usage{
madlib.lda(data, docid, words, topic_num, alpha, eta, iter_num = 20,...)
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
  








}