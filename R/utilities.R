
## ------------------------------------------------------------------------
## utility functions exposed to the users
## ------------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    is(x, "db.data.frame")
}

## ------------------------------------------------------------------------

## cut the data into k pieces
## mainly used for cross-validation
.cut.data <- function (x, k)
{
    ## randomize the data when creating serial index
    y <- .create.inter.table(x, TRUE) 
}
