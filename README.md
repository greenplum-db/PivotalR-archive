PivotalR
=======

PivotalR is a package that enables users of R, the most popular open source statistical programming language
and environment to interact with the [Pivotal (Greenplum) Database](http://www.greenplum.com/products/greenplum-database)
as well as [Pivotal HD](http://www.greenplum.com/products/pivotal-hd) / [HAWQ](http://www.greenplum.com/blog/dive-in/hawq-the-new-benchmark-for-sql-on-hadoop)
and the open-source database [PostgreSQL](http://www.postgresql.org/) for Big Data
analytics. It does so by providing an interface to the operations on tables/views in the database. These
operations are almost the same as those of data.frame. Minimal amount of data is transfered between R and
the database system. Thus the users of R do not need to learn SQL when they
operate on the objects in the database. PivotalR also lets the user to run the functions of the open-source
big-data machine
learning package [MADlib](http://madlib.net/) directly from R.

1. An Introduction to PivotalR

        vignette("pivotalr") # execute in R console to view the PDF file
2. To install PivotalR:
    * Get the latest stable version from CRAN by running `install.packages("PivotalR")`
    * Or try out the latest development version from github by running the following code (Need R >= 3.0.2):

        ```
        ## install.packages("devtools") # 'devtools' package is only available for R >= 3.0.2
        devtools::install_github("PivotalR", "gopivotal")
        ```
    * Or download the source tarball directly from [**here**](https://github.com/gopivotal/PivotalR/tarball/master), and then install the tarball

        ```
        install.packages("gopivotal-PivotalR-xxxx.tar.gz", repos = NULL, type = "source")
        ```
    where "gopivotal-PivotalR-xxxx.tar.gz" is the name of the package that you have downloaded.
3. To get started:
    * [Read the wiki](https://github.com/gopivotal/PivotalR/wiki)
    * [Look at some demo code](https://github.com/gopivotal/PivotalR/wiki/Example)
    * [Watch a training video](https://docs.google.com/file/d/0B9bfZ-YiuzxQc1RWTEJJZ2V1TWc/edit?usp=sharing)
    * [Read the quick-start guide](https://github.com/wjjung317/gp-r/blob/master/docs/PivotalR-quick-start%20v2.pdf)
    * [View the pivotal demo slides](https://docs.google.com/presentation/d/103dv1h4VBCBAixqpezJzWmffyrcRR2h9MCJBTfMOBIM/edit?usp=sharing)
    * [Check out the PivotalR webpage](http://gopivotal.github.io/PivotalR)
