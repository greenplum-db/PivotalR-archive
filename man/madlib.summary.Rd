\name{madlib.summary}
\alias{madlib.summary}
\alias{summary,db.obj-method}
\title{Data summary function}

\description{
`summary' is a generic function used to produce summary statistics of any data
table.  The function invokes particular methods' from the MADlib library to
provide an overview of the data. The
  computation is parallelized by MADlib if the connected database is
  Greenplum database.
}

\usage{
madlib.summary(x, target.cols = NULL, grouping.cols = NULL,
               get.distinct = TRUE, get.quartiles = TRUE,
               ntile = NULL, n.mfv = 10, estimate = TRUE,
               interactive = FALSE)

\S4method{summary}{db.obj}(object, target.cols = NULL, grouping.cols = NULL,
               get.distinct = TRUE, get.quartiles = TRUE,
               ntile = NULL, n.mfv = 10, estimate = TRUE,
               interactive = FALSE)
}

\arguments{
    \item{x,object}{
        An object of \code{db.obj} class. Currently, this parameter is
        mandatory. If it is an object of class \code{db.Rquery} or
        \code{db.view}, a temporary table will be created, and further
        computation will be done on the temporary table. After the
        computation, the temporary will be dropped from the corresponding
        database.
    }
    \item{target.cols}{
        Vector of string. Default value is NULL. Column names in the table for which the summary is desired. When NULL all summary of all columns are returned.
    }
    \item{grouping.cols}{
        List of string. Default value is NULL. Column names in the table by which to group the data. When NULL no grouping of data is performed.
    }
    \item{get.distinct}{
        Logical. Default value is TRUE. Are distinct values required in the summary?
    }
      \item{get.quartiles}{
        Logical. Default value is TRUE. Are quartile values required in the summary?
    }
    \item{ntile}{
        Vector of floats. Default value is NULL. Vector of quantiles required
        as part of the summary.
    }
      \item{n.mfv}{
        Integer. Default value is 10. How many `most-frequent-values' (MFVs) to compute?
    }
    \item{estimate}{
        Logical. Default value is TRUE. Should an estimated computation be used
        to compute values for distincts and MFVs (as opposed to an exact but slow method)?
    }

    \item{interactive}{
        Logical. Default is FALSE. If \code{x} is of type \code{db.view}, then
        extracting data from it would actually compute the view, which might
        take a longer time, especially for large data sets. When
        \code{interactive} is TRUE, this function will ask the user whether to
        continue to extract data from the view.
    }
}


\value{
    A \code{data.frame} object. Each column in the table (or \code{target.cols})
    is a row in the result data frame. Each column of the data frame is described below:

    \item{group_by}{character. Group-by column names (NA if none provided)}
    \item{group_by_value}{character. Values of the group-by columns (NA if no grouping)}
    \item{target_column}{character. Targeted column values for which summary is requested}
    \item{column_number}{integer. Physical column number for the target column in the database}
    \item{data_type}{character. Data type of target column. Standard database descriptors will be displayed}
    \item{row_count}{numeric. Number of rows for the target column}
    \item{distinct_values}{numeric. Number of distinct values in the target column}
    \item{missing_values}{numeric. Number of missing values in the target column}
    \item{blank_values}{numeric. Number of blank values (blanks are defined as values with only whitespace)}
    \item{fraction_missing}{numeric. Percentage of total rows that are missing. Will be expressed as a decimal (e.g. 0.3)}
    \item{fraction_blank}{numeric. Percentage of total rows that are blank. Will be expressed as a decimal (e.g. 0.3)}
    \item{mean}{numeric. Mean value of target column (if target is numeric, else NA)}
    \item{variance}{numeric. Variance of target columns (if target is numeric, else NA for strings)}
    \item{min}{numeric. Min value of target column (for strings this is the length of the shortest string)}
    \item{max}{numeric. Max value of target column (for strings this is the length of the longest string)}
    \item{first_quartile}{numeric. First quartile (25th percentile, valid only for numeric columns)}
    \item{median}{numeric. Median value of target column (valid only for numeric columns)}
    \item{third_quartile}{numeric. Third quartile (75th percentile, valid only for numeric columns)}
    \item{quantile_array}{numeric. Percentile values corresponding to ntile_array}
    \item{most_frequent_values}{character. Most frequent values}
    \item{mfv_frequencies}{character. Frequency of the most frequent
      values}

    The \code{data.frame} has an extra attribute names \code{"summary"},
    which is a \code{\linkS4class{db.data.frame}} object and wraps the
    result table created by MADlib inside the database. One can access
    this object using \code{attr(res, "summary")}, where \code{res} is
    the result of this function.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{madlib.lm}}, \code{\link{madlib.glm}},
    \code{\link{madlib.arima}} are MADlib
    wrapper functions.

    \code{\link{delete}} safely deletes the result of this function.
}

\examples{
\dontrun{
## get the help for a method
## help("madlib.summary")
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", conn.id = cid)
as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)
x  <- db.data.frame("abalone", conn.id = cid, verbose = FALSE)

lk(x, 10)

# madlib.summary
summary_result  <- madlib.summary(x)
print(summary_result)

# madlib.summary
summary_result  <- madlib.summary(x, target.cols=c('rings', 'length', 'diameter'),
                                    grouping.cols=c('sex'),
                                    get.distinct=FALSE,
                                    get.quartiles=TRUE,
                                    ntile=c(0.1, 0.6),
                                    n.mfv=5,
                                    estimate=TRUE,
                                    interactive=FALSE)

print(summary_result)

db.disconnect(cid, verbose = FALSE)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stats}
\keyword{madlib}
