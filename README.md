PivotalR
=======

PivotalR is a package that enables users of R, the most popular open source statistical programming language 
and environment to interact with the [Pivotal (Greenplum) Database](http://www.greenplum.com/products/greenplum-database) 
as well as [Pivotal HD](http://www.greenplum.com/products/pivotal-hd) / [HAWQ](http://www.greenplum.com/blog/dive-in/hawq-the-new-benchmark-for-sql-on-hadoop) for Big Data 
analytics. It does so by providing an interface to the operations on tables/views in the database. These 
operations are almost the same as those of data.frame. Thus the users of R do not need to learn SQL when they 
operate on the objects in the database. 

To install PivotalR:

* Get the latest stable version from CRAN by running `install.packages("PivotalR")`

* Or try out the latest development version from github by running the following code:

    ```R
    # install.packages("devtools")
    devtools::install_github("PivotalR", "madlib-internal")
    ```

To get started:

* [Read the wiki](https://github.com/madlib-internal/PivotalR/wiki)

* [Look at some demo code](https://github.com/madlib-internal/PivotalR/wiki/Example)

* [Watch a training video](https://docs.google.com/file/d/0B9bfZ-YiuzxQc1RWTEJJZ2V1TWc/edit?usp=sharing)

* [Read the quick-start guide](https://github.com/wjjung317/gp-r/blob/master/docs/PivotalR-quick-start%20v2.pdf) 

* [View the pivotal demo slides](https://docs.google.com/presentation/d/103dv1h4VBCBAixqpezJzWmffyrcRR2h9MCJBTfMOBIM/edit?usp=sharing)

* [Check out the PivotalR webpage](http://madlib-internal.github.io/PivotalR)
