## -----------------------------------------------------------------------
## Summary of a db.obj
## -----------------------------------------------------------------------

setClass("summary.madlib")

madlib.summary <- function (x, target.cols = NULL, grouping.cols = NULL,
                            get.distinct = TRUE, get.quartiles = TRUE,
                            ntile = NULL, n.mfv = 10, estimate = TRUE,
                            interactive = FALSE)
{
    ## Only newer versions of MADlib are supported
    .check.madlib.version(x)

    db <- .get.dbms.str(conn.id(x))
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support table summary !")

    if (!is(x, "db.obj"))
        stop("Cannot operate on non db.obj objects!")

    if ((!is.null(target.cols) && !all(target.cols %in% names(x))) ||
        (!is.null(grouping.cols) && !all(grouping.cols %in% names(x))))
        stop("target.cols or grouping.cols has columns that are not in ",
             "the data!")

    warnings <- .suppress.warnings(conn.id(x))

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
            tmp <- as.db.data.frame(x, tbl, FALSE,
                                    is.temp = TRUE,
                                    verbose = interactive,
                                    pivot = FALSE)
        else
            db.q("create temp table", tbl,
                 "as select * from", content(x),
                 conn.id = conn.id(x), verbose = FALSE)
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
                 ".summary('", tbl, "', '", out.tbl, "', ", target.cols,
                 ", ", grouping.cols, ",", get.distinct, ",",
                 get.quartiles, ",", ntile, ",", n.mfv, ",", estimate,
                 ");", sep = "")

    res <- db.q(sql, "select * from", out.tbl,
                nrows = -1, conn.id = conn.id(x), verbose = FALSE)

    if (to.drop.tbl) .db.removeTable(tbl, conn.id(x))

    attr(res, "summary") <- db.data.frame(out.tbl, conn.id = conn.id(x),
                                          verbose = FALSE)

    class(res) <- "summary.madlib"

    .restore.warnings(warnings)

    return (res)
}

## -----------------------------------------------------------------------

.logical.string <- function (str)
{
    if (str)
        "TRUE"
    else
        "FALSE"
}

## -----------------------------------------------------------------------

.text.string <- function (str)
{
    if (is.null(str))
        "NULL::TEXT"
    else
        ## paste("'", paste("\"", str, "\"", collapse = ", ", sep = ""),
        ##       "'", sep = "")
        paste("'", paste(str, collapse = ", ", sep = ""),
              "'", sep = "")
}

## -----------------------------------------------------------------------

.array.string <- function (str, type = "FLOAT8[]")
{
    if (is.null(str))
        paste("NULL::", type, sep = "")
    else
        paste("ARRAY[", paste(str, collapse = ","),
              "]", sep = "")
}

## -----------------------------------------------------------------------

print.summary.madlib <- function (x,
                                  digits = max(3L,
                                  getOption("digits") - 3L), ...)
{
    class(x) <- "data.frame"
    u.group <- unique(x$group_by)
    u.value <- unique(x$group_by_value)

    dat.names <- names(x)[-(1:3)]
    names.len <- nchar(dat.names)
    add <- max(names.len) - names.len
    for (i in seq_len(length(add)))
        dat.names[i] <- paste(dat.names[i],
                              paste(rep(" ", add[i]), collapse = ""),
                              ":", sep = "")

    first.group <- TRUE
    for (g in seq_len(length(x$group_by))) {
        if (!first.group)
            cat("------------------------------------------------\n")
        else
            first.group <- FALSE
        if (!is.na(x$group_by[g])) {
            if (is.na(x$group_by_value[g])) {
                cat("When", x$group_by[g], "= NA\n\n")
            } else {
                cat("When", x$group_by[g], "=", x$group_by_value[g], "\n\n")
            }
        } else {
            cat("For all data:\n\n")
        }

        dat <- x[g, -(1:2)]
        dat.col <- dat[,1]
        dat <- dat[,-1]

        output <- .arrange.summary(dat, dat.col, dat.names,
                                   digits = digits)
        print(format(output, justify = "left"), row.names = F, right = FALSE)
    }
}

## -----------------------------------------------------------------------

show.summary.madlib <- function(object)
{
    print(object)
}

## -----------------------------------------------------------------------

.cut.digits <- c("fraction_missing", "fraction_blank", "mean", "variance",
                 "first_quartile", "median", "third_quartile", "quartile_array",
                 "mfv_frequencies")

.arrange.summary <- function (dat, dat.col, dat.names, digits)
{
    res <- data.frame(tmp = dat.names)
    for (i in seq_len(length(dat.col))) {
        tmp <- rep("", length(dat.names))
        for (j in seq_len(length(dat.names))) {
            if (is.na(dat[i,j]))
                tmp[j] <- paste(dat.names[j], "NA")
            else {
                if (dat.names[j] %in% .cut.digits) {
                    nums <- as.vector(arraydb.to.arrayr(as.character(dat[i,j]), "double"))
                    nums.str <- paste(format(nums, digits = digits), collapse = ", ")
                    tmp[j] <- paste(dat.names[j], nums.str)
                } else {
                    tmp[j] <- paste(dat.names[j],
                                    paste(as.vector(arraydb.to.arrayr(as.character(dat[i,j]), "character")),
                                          collapse = ", "))
                }
            }
        }
        res[[dat.col[i]]] <- tmp
    }
    res <- res[-1]
    ## attr(res, "row.names") <- ""
    return (res)
}

## -----------------------------------------------------------------------

setGeneric("summary")

setMethod("summary",
    signature(object = "db.obj"),
    function (object, target.cols = NULL, grouping.cols = NULL,
              get.distinct = TRUE, get.quartiles = TRUE,
              ntile = NULL, n.mfv = 10, estimate = TRUE,
              interactive = FALSE) {
        madlib.summary(object, target.cols, grouping.cols,
                       get.distinct, get.quartiles,
                       ntile, n.mfv = 10, estimate,
                       interactive)
    })
