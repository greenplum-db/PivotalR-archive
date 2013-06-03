\name{PivotalR-package}
\alias{PivotalR-package}
\docType{package}
\title{
  Wrapper for MADlib, an open-source library for scalable in-database
  analytics
}
\description{
  PivotalR is a package that enables users of R, the most popular open source statistical 
programming language and environment to interact with the Pivotal (Greenplum) 
Database as well as Pivotal HD/HAWQ for Big Data analytics. It does so
by providing an interface to the operations on tables/views in the
database. These operations are almost the same as those of
data.frame. Thus the users of R do not need to learn SQL when they
operate on the objects in the database.
}

\details{
\tabular{ll}{
Package: \tab PivotalR\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2013-04-01\cr
License: \tab GPL (>= 2)\cr
Depends: \tab methods, DBI, RPostgreSQL\cr
}

This package enables R users to easily develop, refine and deploy R
scripts that leverage the 
parallelism and scalability of the database as well as in-database analytics libraries to 
operate on big data sets that would otherwise not fit in R memory - all this without 
having to learn SQL because the package provides an interface that they are familiar with.

  The package also provides a wrapper for MADlib. MADlib is an open-source
  library for scalable in-database analytics. It provides data-parallel
  implementations of mathematical, statistical and machine-learning
  algorithms for structured and unstructured data. The number of machine
  learning algorithms that MADlib covers is quickly increasing.

As an R front-end to the PostgreSQL-like databases, this package
minimizes the amount of data transferred between the database and R. All
the big data is stored in the database. The
user enters their familiar R syntax, and the package translates it into
SQL queries and sends the SQL query into database for parallel
execution. The computation result, which is small (if it is as big as
the original data, what is the point of big data analytics?), is
returned to R to the user.

On the other hand, this package also gives the usual SQL users the
access of utilizing the powerful analytics and graphics functionalities
of R. Although the database itself has difficulty in plotting, the
result can be analyzed and presented beautifully with R. 

This current version of PivotalR provides the core R infrastructure and data frame functions 
as well as over 50 analytical functions in R that leverage in-database execution. These 
include

* Data Connectivity - db.connect, db.disconnect, db.Rquery

* Data Exploration - db.data.frame, subsets

* R language features - dim, names, min, max, nrow, ncol, summary etc

* Reorganization Functions - merge, by (group-by), samples

* Transformations - as.factor, null replacement

* Algorithms - linear regression and logistic regression wrappers for MADlib,
}

\note{
  This package is differernt from PL/R, which is another way of using R
  with PostgreSQL-like databases. PL/R enables the users to run R
  scripts from SQL. In the parallel Greenplum database, one can use PL/R
  to implement parallel algorithms.

  However, PL/R still requires non-trivial knowledge of SQL to use it
  effectively. It is mostly limited to explicitly parallel jobs. And for
  the end user, it is still a SQL interface.

  This package does not require any knowledge of SQL, and it works for
  both explicitly and implicitly parallel jobs by employing the
  open-source MADlib library. It is much more scalable.
  And for the end user, it is a pure R
  interface with the conventional R syntax.
}

\author{
  Author: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net},  with contributions
  from Woo Jung \email{Woo.Jung@gopivotal.com}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}
\references{
  [1] MADlib website, \url{http://madlib.net}
  
  [2] MADlib user docs, \url{http://doc.madlib.net/master}

  [3] MADlib Wiki page, \url{http://github.com/madlib/madlib/wiki}

  [4] MADlib contribution guide,
  \url{https://github.com/madlib/madlib/wiki/Contribution-Guide}

  [5] MADlib on GitHub, \url{https://github.com/madlib/madlib}
}

\seealso{
  \code{\link{madlib.lm}} Linear regression

  \code{\link{madlib.glm}} Linear, logistic and multinomial logistic
  regressions

  \code{\link{madlib.summary}} summary of a table in the database.
}

\examples{
\dontrun{
## get the help for one method
help("preview,db.Rquery-method")

## create multiple connections to different databases
db.connect(port = 5433) # connection 1, use default values for the parameters
db.connect(dbname = "test", user = "qianh1", password = "", host =
"remote.machine.com", madlib = "madlib07", port = 5432) # connection 2

db.list() # list the info for all the connections


}
}

\keyword{ package }