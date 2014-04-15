\name{abalone}

\alias{abalone}

\docType{data}

\title{
  Abalone data set
}

\description{
  An example \code{data.frame} which is used by examples in this user manual
}

\usage{data(abalone)}

\format{
  Given is the attribute name, attribute type, the measurement unit and a brief description. The number of rings is the value to predict: either as a continuous value or as a classification problem.

  Name / Data Type / Measurement Unit / Description

-----------------------------

Id / integer / -- / index of each observation

Sex / nominal / -- / M, F, and I (infant)

Length / continuous / mm / Longest shell measurement

Diameter	/ continuous / mm / perpendicular to length

Height / continuous / mm / with meat in shell

Whole weight / continuous / grams / whole abalone

Shucked weight / continuous  / grams / weight of meat

Viscera weight / continuous / grams / gut weight (after bleeding)

Shell weight / continuous / grams / after being dried

Rings / integer / -- / +1.5 gives the age in years
}

\note{Lazy data loading is enabled in this package. So the user does not
need to explicitly run \code{data(abalone)} to load the data. It will be
loaded whenever it is used.}

\details{
  Predicting the age of abalone from physical measurements. The age of
  abalone is determined by cutting the shell through the cone, staining
  it, and counting the number of rings through a microscope -- a boring
  and time-consuming task. Other measurements, which are easier to
  obtain, are used to predict the age. Further information, such as
  weather patterns and location (hence food availability) may be
  required to solve the problem.

From the original data examples with missing values were removed (the
majority having the predicted value missing), and the ranges of the
continuous values have been scaled for use with an ANN (by dividing by
200).
}
\source{
  [1] The original data is downloaded from \url{http://archive.ics.uci.edu/ml/datasets/Abalone}

  [2] Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and Wes B Ford (1994)
"The Population Biology of Abalone (_Haliotis_ species) in Tasmania. I. Blacklip Abalone (H. rubra) from the North Coast and Islands of Bass Strait",
Sea Fisheries Division, Technical Report No. 48 (ISSN 1034-3288)
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name

## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
## The user does not need to run data(abalone) to load the data
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", key = "id",
                      distributed.by = "id", conn.id = cid,
                      verbose = FALSE)

## preview the actual data
lk(x)

## preview the actual data ordered by id
lk(sort(x, FALSE, x$id))

db.disconnect(cid, verbose = FALSE)
}
}
\keyword{datasets}
