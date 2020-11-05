PivotalR
=======

PivotalR is a package that enables users of R, the most popular open source statistical programming language
and environment, to interact with [Greenplum Database](https://greenplum.org/)
and the [PostgreSQL](https://www.postgresql.org/) for big data
analytics. It does so by providing an interface to the operations on tables/views in the database. These
operations are almost the same as those of data.frame. Minimal amount of data is transfered between R and
the database. Thus the users of R do not need to learn SQL when they
operate on the objects in the database. PivotalR also lets the user to run the functions of the open source
machine learning package [Apache MADlib](https://madlib.apache.org/) directly from R.

1. An Introduction to PivotalR

        vignette("pivotalr") # execute in R console to view the PDF file
2. To install PivotalR:
    * Get the latest stable version from CRAN by running `install.packages("PivotalR")`
    * Or try out the latest development version from github by running the following code (need R >= 3.0.2):

        ```
        ## install.packages("devtools") # 'devtools' package is only available for R >= 3.0.2
        devtools::install_github("PivotalR", "greenplum-db")
        ```
    * Or download the source tarball directly from [**here**](https://github.com/greenplum-db/PivotalR/tarball/master), and then install the tarball

        ```
        install.packages("greenplum-db-PivotalR-xxxx.tar.gz", repos = NULL, type = "source")
        ```
    where "greenplum-db-PivotalR-xxxx.tar.gz" is the name of the package that you have downloaded.
3. To get started:
    * [Read the wiki](https://github.com/greenplum-db/PivotalR/wiki)
    * [Look at some demo code](https://github.com/greenplum-db/PivotalR/wiki/Example)
    * [Watch a training video](https://www.youtube.com/watch?v=6cmyRCMY6j0)
    * [Read the quick-start guide](https://github.com/wjjung317/gp-r/blob/master/docs/PivotalR-quick-start%20v2.pdf)
