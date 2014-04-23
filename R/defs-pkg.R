
## -----------------------------------------------------------------------
## Private variables of the package
## -----------------------------------------------------------------------

## If we decide to change the package name,
## this definition will make things easier,
## because we do not directly refer to the name
## in the package, instead we use this constant.
.this.pkg.name <- "PivotalR"

## -----------------------------------------------------------------------

## Which R packages are used for connection
## We might want to support multiple packages
.supported.connections <- c(# "RODBC", # not supported yet
                            "RPostgreSQL")

## -----------------------------------------------------------------------

.err.class <- "try-error"

## -----------------------------------------------------------------------

## All local variables defined at the package loading time
## cannot be changed without exposing to users. If we really
## export these variables, they will easily interfere with other
## user defined variables.
##
## The only way is to define a local environment inside the package.
##
## The environment is a constant and cannot be changed,
## but the variables inside this environment can be changed.
## The environment constant is not exported, and is thus hidden
## from the users.
.localVars <- new.env()
## .localVars has the following variables inside it
## (1) installed.pkgs - all packages avilable in user's path
## (2) db - a list of connection info
## (3) conn.type - a list with vector element, contains connection pkg
## for each conn id
## (4) drv - drivers for each connection packages


## Use this to represent NULL
## user is extremely unlikely to use this name
.null.string <- "<@#$NULL%^&>"

## -----------------------------------------------------------------------

.num.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint", "decimal", "numeric", "double precision",
                "float8", "real", "serial", "bigserial")

## --

.int.types <- c("smallint", "integer", "int2", "int4", "int4",
                "bigint")

## --

.txt.types <- c("character varying", "varchar", "character",
                "char", "text")

## --

.time.types <- c("timestamp", "time", "date", "interval",
                 "timestamp with time zone")

.udt.time.types <- c("timestamp", "time", "date", "interval", "timestamptz")

.time.change <- c("interval", "time", "integer", "interval", "interval")


## -----------------------------------------------------------------------
## create class structure
## -----------------------------------------------------------------------

## The R object has its corresponding table/view in database
## object in database
setClass("db.data.frame",
         representation(
             .name = "character", # c(schema_name, table/view_name)
             .content = "character", # object name (schema.table)
             .conn.id = "numeric", # connection ID
             ## table properties
             .col.name = "character", # column names
             .col.data_type = "character", # column types
             .col.udt_name = "character",
             .table.type = "character", # is the object temp ?
             .factor.suffix = "character",
             .appear.name = "character",
             .dummy = "character",
             .dummy.expr = "character",
             .is.factor = "logical",
             .factor.ref = "character",
             .dist.by = "character"
             )
         )

## table, a sub-class of db.obj
setClass("db.table",
         representation(
             .key = "character", # which column is used to
             ## identify different rows, i.e. index
             .dim = "numeric" # dimension of table
             ),
         contains = "db.data.frame")

## view, a sub-class of db.obj
setClass("db.view",
         representation(
             .key = "character"
             ),
         contains = "db.data.frame")

## -----------------------------------------------------------------------

## The R object has no coresponding table in database
## It is generated in the middle of computations that involve
## "db.obj" or "query.obj" objects.
## It can be converted to "db.obj" via realize(), which returns
## a db.obj
## More precisely, it is a view existing in R only
## It can be converted into db.obj objects
## The object is equivalent to
## SELECT paste(.expr, collapse = ",") FROM .parent;
setClass("db.Rquery",
         representation(
             .content = "character",
             .expr = "character",
             .source = "character", # the original table object
             .parent = "character", # name of its parent object
             .conn.id = "numeric",
             .col.name = "character",
             .key = "character", # identification column
             .col.data_type = "character", # column types
             .col.udt_name = "character",
             .where = "character", # WHERE clause
             .is.factor = "logical", # a boolean vector
             .factor.suffix = "character",
             .factor.ref = "character",
             .sort = "list", # order by
             .is.agg = "logical", # is this an aggrgate?
             .dist.by = "character"
             ),
         prototype = list(.is.agg = FALSE)
         )

## -----------------------------------------------------------------------

## matrix representation
setClass("db.Rcrossprod",
         representation(
             .is.crossprod = "logical", # is the column a crossprod
             .is.symmetric = "logical", # is the column crossprod symmetric
             .dim = "numeric"),
         ## .inverse = "logical"),
         ## prototype = list(.inverse = FALSE),
         contains = "db.Rquery")

## ----------------------------------------------------------------------

## A db.Rquery, but is treated as a view
setClass("db.Rview",
         representation(
             .sub = "character"),
         contains = "db.Rquery")

## Convert a db.Rquery object to db.Rview object
as.db.Rview <- function (x) {
    if (!is(x, "db.Rquery"))
        stop(deparse(substitute(x)), " must be a db.Rquery object!")
    w <- as(x, "db.Rview")
    w@.parent <- x@.content
    w@.expr <- "\"" %+% w@.col.name %+% "\""
    w@.sub <- gsub("__", "", .unique.string())
    w@.content <- paste("select ",
                        paste(w@.expr, "as \"", w@.col.name, "\"",
                              collapse = ", "),
                        " from (", w@.parent, ") ", w@.sub, sep = "")
    w@.where <- ""
    w@.sort <- list(by = "", order = "", str = "")
    w
}

## -----------------------------------------------------------------------

## Abstract interface, which is the parent of both classes
## defined in the above.
## Many functions in this package should operate on both classes.
## So we define this abstract class.
setClassUnion("db.obj", members = c("db.data.frame", "db.Rquery"))
