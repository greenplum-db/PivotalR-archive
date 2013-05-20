
## ------------------------------------------------------------------------
## Summary of a db.obj
## ------------------------------------------------------------------------

madlib.summary <- function (x, target.cols = NULL, grouping.cols = NULL,
                            get.distinct = TRUE, get.quartiles = TRUE,
                            ntile = NULL, n.mfv = 10, estimate = TRUE,
                            interactive = TRUE)
{
    ## Only newer versions of MADlib are supported
    if (identical(.localVars$db[[idx]]$madlib.v, numeric(0)) ||
        .madlib.version.number(conn.id(data)) < 0.6)
        stop("MADlib error: Please use Madlib version newer than 0.5!")
    
    if (!is(x, "db.obj"))
        stop("Cannot operate on non db.obj objects!")

    if ((!is.null(target.cols) && !all(target.cols %in% names(x))) ||
        (!is.null(grouping.cols) && !all(grouping.cols %in% names(x))))
        stop("target.cols or grouping.cols has columns that are not in ",
             "the data!")
    
    if (is(x, "db.view") || is(x, "db.Rquery")) {
        if (interactive) {
            cat(deparse(substitute(x)),
                "does not point to a table in the database",
                dbname(conn.id(x)),
                "and it might take time to create a table and get the summary for this!\n")
            go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                              c("yes", "y", "no", "n"))
            if (go == "no" || go == "n") return
        }
        tbl <- .unique.string()
        to.drop.tbl <- TRUE
        if (is(x, "db.Rquery"))
            tmp <- as.db.data.frame(x, tbl, conn.id(x), FALSE
                                    is.temp = TRUE,
                                    verbose = interactive,
                                    pivot.factor = FALSE)
        else
            .db.getQuery(paste("create temp table", tbl,
                               "as select * from", content(x)), conn.id(x))
    } else {
        tbl <- content(x)
        to.drop.tbl <- FALSE
    }

    out.tbl <- .unique.string()

    ## create SQL query
    target.cols <- .text.string(target.cols)
    grouping.cols <- .text.string(grouping.cols)
    get.distinct <- .logical.string(get.distinct)
    get.quartiles <- .logical.string(get.quartiles)
    ntile <- .array.string(ntile, "FLOAT8[]")
    estimate <- .logical.string(estimate)
    
    sql <- paste("SELECT ", schema.madlib(conn.id(x)),
                 ".summary(", tbl, ",", out.tbl, ",", target.cols,
                 ",", grouping.cols, ",", get.distinct, ",",
                 get.quartiles, ",", ntile, ",", n.mfv, ",", estimate,
                 ");", sep = "")

    res <- try(.db.getQuery(sql, conn.id(x)))
    if (is(res, ".err.class"))
        stop("Could not do the summary!")
    
    .db.removeTable(out.tbl, conn.id(x))
    if (to.drop.tbl) .db.removeTable(tbl, conn.id(x))

    class(res) <- "summary.madlib"
}

## ------------------------------------------------------------------------

.logical.string <- function (str)
{
    if (str)
        "TRUE"
    else
        "FALSE"
}

## ------------------------------------------------------------------------

.text.string <- function (str)
{
    if (is.null(str))
        "NULL::TEXT"
    else 
        paste("'", paste(str, collapse = ","),
              "'", sep = "")
}

## ------------------------------------------------------------------------

.array.string <- function (str, type = "FLOAT8[]")
{
    if (is.null(str))
        paste("NULL::", type, sep = "")
    else
        paste("ARRAY[", paste(str, collapse = ","),
              "]", sep = "")
}

## ------------------------------------------------------------------------

print.summary.madlib <- function (x,
                                  digits = max(3L,
                                  getOption("digits") - 3L), ...)
{
    u.group <- unique(x$group_by_column)
    u.value <- unique(x$group_by_value)

    dat.names <- names(x)[-(1:3)]
    names.len <- nchar(dat.names)
    add <- max(names.len) - names.len
    for (i in seq_len(length(add)))
        dat.names[i] <- paste(dat.names[i],
                              paste(rep(" ", add[i]), collapse = ""),
                              ":", sep = "")

    for (g in u.group) {
        if (is.na(g))
            gb <- is.na(g)
        else
            gb <- x$group_by_column == g
        for (v in u.value) {
            if (is.na(v))
                vb <- is.na(v)
            else
                vb <- x$group_by_value == v

            dat <- x[gb & vb, -(1:2)]
            dat.col <- dat[,1]
            dat <- dat[,-1]

            output <- .arrange.summary(dat, dat.col, dat.names,
                                       digits = digits)
            
        }
    }
}

## ------------------------------------------------------------------------

.arrange.summary <- function (dat, dat.col, dat.names, digits)
{
    res <- data.frame(tmp = dat.names)
    for (i in seq_len(length(dat.col))) {
        tmp <- rep("", length(dat.names))
        for (j in seq_len(length(dat.names))) {
            if (is.na(dat[i,j]))
                tmp[j] <- paste(dat.names[j], "NA")
            else
                tmp[j] <- paste(dat.names[j],
                                paste(as.vector(arraydb.to.arrayr(dat[i,j], "character")),
                                      collapse = ", "))
        }
        res[[dat.col[i]]] <- tmp
    }
    res <- res[-1]
    attr(res, "row.names") <- dat.names
    return (res)
}
