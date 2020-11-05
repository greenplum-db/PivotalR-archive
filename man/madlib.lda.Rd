\name{madlib.lda}
\alias{madlib.lda}

\title{
  Wrapper for MADlib's Latent Dirichilet Allocation
}

\description{
  This function is a wrapper for MADlib's Latent Dirichlet Allocation. The
  computation is parallelized by MADlib if the connected database is
  distributed. Please refer to MADlib documentation for details of the
  algorithm implementation [1].
}

\usage{
  madlib.lda(data, topic_num, alpha, beta, iter_num = 20,
  nstart = 1, best = TRUE,...)
}

\arguments{

  \item{data}{
      An object of \code{db.obj} class. This is the database table
      containing the documents on which the algorithm will train.
      The text of each document should be tokenized into 'words'.
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

  \item{nstart}{
      Number of repeated random starts.
  }

  \item{best}{
      If TRUE only the model with the minimum perplexity is returned.
  }

  \item{\dots}{
    Other optional parameters. Not implemented.
  }

}

\value{
  An \code{lda.madlib} object or a list of them, which is a list that
  contains the following items:

  \item{assignments}{
      The per-document topic assignments.
  }
  \item{document_sums}{
      The per-document topic counts.
  }
  \item{model_table}{
      The \code{db.table} object for accessing the model table in the database.
  }
  \item{output_table}{
      The \code{db.table} object for accessing the output table in the database.
  }
  \item{tf_table}{
      The \code{db.table} object for accessing the term frequency table in the
      database.
  }
  \item{topic_sums}{
      The per-topic sum of assignments.
  }
  \item{topics}{
      The per-word association with topics.
  }
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}
\references{
  [1] Documentation of LDA in the latest MADlib release,
    \url{https://madlib.apache.org/docs/latest/group__grp__lda.html}
}
\seealso{

  \code{predict.lda.madlib} is used for prediction-labelling test documents
  using a learned \code{lda.madlib} model.

  \code{perplexity.lda.madlib} is used for computing the perplexity of a
  learned \code{lda.madlib} model.

}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

dat <- db.data.frame("__madlib_pivotalr_lda_data__", conn.id = cid,
  verbose = FALSE)

output.db <- madlib.lda(dat, 2,0.1,0.1, 50)

perplexity.db <- perplexity.lda.madlib(output.db)
print(perplexity.db)

## Run LDA multiple times and get the best one
output.db <- madlib.lda(dat, 2,0.1,0.1, 50, nstart=2)
perplexity.db <- perplexity.lda.madlib(output.db)
print(perplexity.db)

## Run LDA multiple times and keep all models
output.db <- madlib.lda(dat, 2,0.1,0.1, 50, nstart=2, best=FALSE)

perplexity.db <- perplexity.lda.madlib(output.db[[1]])
print(perplexity.db)

perplexity.db <- perplexity.lda.madlib(output.db[[2]])
print(perplexity.db)

db.disconnect(cid)
}
}

\keyword{madlib}
