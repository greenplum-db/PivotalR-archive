PivotalR
=======

PivotalR is a package that enables users of R, the most popular open source statistical programming language 
and environment to interact with the [Pivotal (Greenplum) Database](http://www.greenplum.com/products/greenplum-database) 
as well as [Pivotal HD](http://www.greenplum.com/products/pivotal-hd) / [HAWQ](http://www.greenplum.com/blog/dive-in/hawq-the-new-benchmark-for-sql-on-hadoop) for Big Data 
analytics. It does so by providing an interface to the operations on tables/views in the database. These 
operations are almost the same as those of data.frame. Thus the users of R do not need to learn SQL when they 
operate on the objects in the database. 

A training video and a quick-start guide are available at [Quick Start](http://zimmeee.github.io/gp-r/#pivotalr).

See [Example](https://github.com/madlib-internal/PivotalR/wiki/Example) to get some flavour of PivotalR


This package enables R users to easily develop, refine and deploy R scripts that leverage the parallelism and 
scalability of the database as well as in-database analytics libraries to operate on big data sets that would 
otherwise not fit in R memory - all this without having to learn SQL because the package provides an interface 
that they are familiar with.

The package also provides a wrapper for [MADlib](http://madlib.net/). [MADlib](http://madlib.net/) is an open-source library for scalable in-database 
analytics. It provides data-parallel implementations of mathematical, statistical and machine-learning 
algorithms for structured and unstructured data. The number of machine learning algorithms that MADlib covers 
is quickly increasing.

As an R front-end to the PostgreSQL-like databases, this package minimizes the amount of data transferred 
between the database and R. All the big data is stored in the database. The user enters their familiar R 
syntax, and the package translates it into SQL queries and sends the SQL query into database for parallel 
execution. The computation result, which is small (if it is as big as the original data, what is the point 
of big data analytics?), is returned to R to the user.

On the other hand, this package also gives the usual SQL users the access of utilizing the powerful analytics 
and graphics functionalities of R. Although the database itself has difficulty in plotting, the result can be 
analyzed and presented beautifully with R.

This current version of PivotalR provides the core R infrastructure and data frame functions as well as over 
50 analytical functions in R that leverage in-database execution. These include

* Data Connectivity - db.connect, db.disconnect, db.Rquery

* Data Exploration - db.data.frame, subsets

* R language features - dim, names, min, max, nrow, ncol, summary etc

* Reorganization Functions - merge, by (group-by), samples

* Transformations - as.factor, null replacement

* Algorithms - linear regression and logistic regression wrappers for MADlib,
