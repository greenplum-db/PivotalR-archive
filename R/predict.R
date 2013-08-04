
## ------------------------------------------------------------------------
## Predict
## ------------------------------------------------------------------------

predict.lm.madlib <- function (object, newdata, ...)
{
    .predict(object, newdata, "linregr_predict", "double precision", "float8")
}

predict.lm.madlib.grps <- function (object, newdata, ...)
{
    .predict(object, newdata, "linregr_predict", "double precision", "float8")
}

## ------------------------------------------------------------------------

predict.logregr.madlib <- function (object, newdata, ...)
{
    .predict(object, newdata, "logregr_predict", "boolean", "bool")
}

predict.logregr.madlib.grps <- function (object, newdata, ...)
{
    .predict(object, newdata, "logregr_predict", "boolean", "bool")
}

## ------------------------------------------------------------------------

.predict <- function (object, newdata, func.str, data.type, udt.name)
{
    if (is(object, "lm.madlib") || is(object, "logregr.madlib"))
        object <- list(object)
    
    if (!is(newdata, "db.obj"))
        stop("New data for prediction must be a db.obj!")

    madlib <- schema.madlib(conn.id(newdata))
    if (is(newdata, "db.data.frame")) {
        tbl <- content(newdata)
        src <- tbl
        parent <- src
        where <- ""
        where.str <- ""
        sort <- list(by = "", order = "", str = "")
    } else {
        if (newdata@.source == newdata@.parent)
            tbl <- newdata@.parent
        else
            tbl <- paste("(", newdata@.parent, ") s", sep = "")
        src <- newdata@.source
        parent <- newdata@.parent
        where <- newdata@.where
        if (where != "") where.str <- paste(" where", where)
        else where.str <- ""
        sort <- newdata@.sort
    }
    if (!is(newdata, "db.data.frame"))
        ind.str <- .replace.col.with.expr(object[[1]]$ind.str,
                                          names(newdata),
                                          newdata@.expr)
    else
        ind.str <- object[[1]]$ind.str
    
    ## deal with groups
    coef.i <- which(names(object[[1]]) == "coef")
    grp.col <- names(object[[1]])[seq_len(coef.i - 1)]

    if (length(object[[1]]$grp.cols) == 0) {
        coef <- paste("array[", paste(object[[1]]$coef, collapse = ", "),
                      "]",
                      sep = "")
        expr <- paste(madlib, ".", func.str, "(", coef, ", ",
                      ind.str, ")", sep = "")
    } else {
        l <- length(object[[1]]$grp.cols)
        expr <- "case when "
        n <- length(object)
        for (i in seq_len(n)) {
            tmp <- ""
            if (i != n) {
                for (j in seq_len(l)) {
                    tmp <- paste(tmp, object[[i]]$grp.cols[j], " = '",
                                 object[[i]][[object[[i]]$grp.cols[j]]],
                                 "'::",
                                 newdata@.col.data_type[which(
                                     names(newdata) == object[[i]]$grp.cols[j])],
                                 sep = "")
                    if (j != l) tmp <- paste(tmp, " and ", sep = "")
                }
                if (!is(newdata, "db.data.frame"))
                    tmp <- .replace.col.with.expr(tmp, names(newdata),
                                                  newdata@.expr)
                expr <- paste(expr, tmp, " then ", sep = "")
            }
            coef.i <- paste("array[", paste(object[[i]]$coef,
                                            collapse = ", "),
                            "]", sep = "")
            expr <- paste(expr, madlib, ".", func.str, "(", coef.i, ", ",
                          ind.str, ")", sep = "")
            if (i < n - 1)
                expr <- paste(expr, " when ", sep = "")
            else if (i == n - 1)
                expr <- paste(expr, " else ", sep = "")
            else
                expr <- paste(expr, " end", sep = "")
        }    
    }
    
    sql <- paste("select ", expr, " as madlib_predict from ",
                 tbl, where.str, sort$str, sep = "")

    if (length(object[[1]]$dummy) != 0) {
        for (i in seq_len(length(object[[1]]$dummy))) {
            sql <- gsub(paste("(\"", object[[1]]$dummy[i], "\"|",
                              object[[1]]$dummy[i], ")", sep = ""),
                        object[[1]]$dummy.expr[i], sql)
            expr <- gsub(paste("(\"", object[[1]]$dummy[i], "\"|",
                               object[[1]]$dummy[i], ")", sep = ""),
                         object[[1]]$dummy.expr[i], expr)
        }
    }

    new("db.Rquery",
        .content = sql,
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(newdata),
        .col.name = "madlib_predict",
        .key = character(0),
        .col.data_type = data.type,
        .col.udt_name = udt.name,
        .where = where,
        .is.factor = FALSE,
        .factor.suffix = "",
        .sort = sort)
}

## ------------------------------------------------------------------------

.replace.col.with.expr <- function (str, cols, expr)
{
    for (i in seq_len(length(expr))) {
        ep <- expr[i]
        col <- cols[i]
        str <- gsub(paste("([\\+\\-\\*/\\s%]|^)", col,
                          "([\\+\\-\\*/\\s%]|$)", sep = ""),
                    paste("\\1", ep, "\\2", sep = ""),
                    str, perl = TRUE)
    }
    str
}
