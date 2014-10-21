## -----------------------------------------------------------------------
## Predict
## -----------------------------------------------------------------------

predict.lm.madlib <- function (object, newdata, ...)
{
    .predict(object, newdata, "elastic_net_gaussian_predict",
             "double precision", "float8")
}

predict.lm.madlib.grps <- function (object, newdata, ...)
{
    .predict(object, newdata, "elastic_net_gaussian_predict",
             "double precision", "float8")
}

## -----------------------------------------------------------------------

predict.logregr.madlib <- function (object, newdata,
                                    type = c("response", "prob"), ...)
{
    type <- match.arg(type)
    if (type == "response")
        .predict(object, newdata, "elastic_net_binomial_predict", "boolean",
                 "bool")
    else {
        .predict.prob(object, newdata) # only for logistic regression
    }
}

## ------------------------------------------------------------

predict.logregr.madlib.grps <- function (object, newdata,
                                         type = c("response", "prob"), ...)
{
    type <- match.arg(type)
    if (type == "response")
        .predict(object, newdata, "elastic_net_binomial_predict", "boolean",
                 "bool")
    else {
        .predict.prob(object, newdata) # only for logistic regression
    }
}

## ------------------------------------------------------------

## predict function for all GLM functions

predict.glm.madlib <- function(object, newdata = object$data,
                               type = c('response', 'prob'), ...)
{
    type <- match.arg(type)
    family.name <- object$family
    link.name <- object$link

    if (family.name == 'binomial') {
        if (type == 'response') { # TRUE or FALSE
            .predict(object, newdata, 'glm_predict_binomial', 'boolean', 'bool',
                     extra = paste(",", "'", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else { # type == 'prob'
            .predict(object, newdata, 'glm_predict', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        }
    } else if (family.name == 'poisson') {
        if (type == 'response') {
            .predict(object, newdata, 'glm_predict_poisson', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else {
            stop("Family poisson does not support the prediction of prob type!")
        }
    } else {
        if (type == 'response') {
            .predict(object, newdata, 'glm_predict', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else {
            stop("Family ", family.name, " does not support the prediction of prob type!")
        }
    }
}

## ------------------------------------------------------------

predict.glm.madlib.grps <- function(object, newdata = object[[1]]$data,
                                    type = c('response', 'prob'), ...)
{
    type <- match.arg(type)
    family.name <- object[[1]]$family
    link.name <- object[[1]]$link

    if (family.name == 'binomial') {
        if (type == 'response') { # TRUE or FALSE
            .predict(object, newdata, 'glm_predict_binomial', 'boolean', 'bool',
                     extra = paste(",", "'", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else { # type == 'prob'
            .predict(object, newdata, 'glm_predict', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        }
    } else if (family.name == 'poisson') {
        if (type == 'response') {
            .predict(object, newdata, 'glm_predict_poisson', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else {
            stop("Family poisson does not support the prediction of prob type!")
        }
    } else {
        if (type == 'response') {
            .predict(object, newdata, 'glm_predict', 'double precision', 'float8',
                     extra = paste(", '", link.name, "'", sep = ''),
                     with.intercept = FALSE)
        } else {
            stop("Family ", family.name, " does not support the prediction of prob type!")
        }
    }
}



## ----------------------------------------------------------------------

## predict probability only for binomial models
.predict.prob <- function(object, newdata,
                          func.str = "elastic_net_binomial_prob",
                          extra = "")
{
    if (is(object, "logregr.madlib")) object <- list(object)
    if (!is(newdata, "db.obj"))
        stop("New data for prediction must be a db.obj!")

    db <- .get.dbms.str(conn.id(newdata))
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support this!")
    madlib <- schema.madlib(conn.id(newdata))

    strs <- .get.extra.str(newdata)
    tbl <- strs$tbl
    where <- strs$where
    where.str <- strs$where.str
    sort <- strs$sort
    src <- strs$src
    parent <- strs$parent
    func.str <- paste(madlib, ".", func.str, sep = "")

    if (!is(newdata, "db.data.frame")) {
        if (length(object[[1]]$dummy) != 0) {
            l <- length(names(newdata))
            for (i in seq_len(length(object[[1]]$dummy))) {
                newdata[[object[[1]]$dummy[i]]] <- 1
                newdata@.expr[l+i] <- object[[1]]$dummy.expr[i]
            }
        }
        ind.vars <- .replace.col.with.expr1(object[[1]]$ind.vars, newdata)
    } else
        ind.vars <- object[[1]]$ind.vars
    ind.str <- paste("array[", paste(ind.vars, collapse = ","), "]", sep = "")

    if (length(object) == 1) {
        if (object[[1]]$has.intercept) {
            coef <- object[[1]]$coef[-1]
            intercept <- object[[1]]$coef[1]
        } else {
            coef <- object[[1]]$coef
            intercept <- 0
        }
        coef <- paste("array[", paste(coef, collapse = ", "), "]", sep = "")
        expr <- paste(func.str, "(", coef, ", ", intercept, ", ",
                      ind.str, extra, ")", sep = "")
    } else {
        l <- length(object[[1]]$grp.cols)
        expr <- "case when "
        n <- length(object)
        for (i in seq_len(n)) {
            tmp <- object[[i]]$data@.where
            if (!is(newdata, "db.data.frame"))
                tmp <- .replace.col.with.expr1(gsub(" = ", " == ", tmp),
                                               newdata)
            expr <- paste(expr, tmp, " then ", sep = "")

            if (object[[i]]$has.intercept) {
                coef <- object[[i]]$coef[-1]
                intercept <- object[[i]]$coef[1]
            } else {
                coef <- object[[i]]$coef
                intercept <- 0
            }
            coef.i <- paste("array[", paste(coef, collapse = ", "), "]",
                            sep = "")
            expr <- paste(expr, func.str, "(", coef.i, ", ", intercept, ", ",
                          ind.str, extra, ")", sep = "")

            if (i < n)
                expr <- paste(expr, " when ", sep = "")
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
        .col.data_type = "double precision",
        .col.udt_name = "float8",
        .where = where,
        .is.factor = FALSE,
        .factor.ref = as.character(NA),
        .factor.suffix = "",
        .sort = sort,
        .dist.by = newdata@.dist.by)
}

## -----------------------------------------------------------------------

.predict <- function (object, newdata, func.str, data.type, udt.name, extra = "",
                      with.intercept = TRUE)
{
    if (is(object, "lm.madlib") || is(object, "logregr.madlib") || is(object, 'glm.madlib'))
        object <- list(object)

    if (!is(newdata, "db.obj"))
        stop("New data for prediction must be a db.obj!")

    db <- .get.dbms.str(conn.id(newdata))
    madlib <- schema.madlib(conn.id(newdata))

    strs <- .get.extra.str(newdata)
    tbl <- strs$tbl
    where <- strs$where
    where.str <- strs$where.str
    sort <- strs$sort
    src <- strs$src
    parent <- strs$parent

    if (!is(newdata, "db.data.frame")) {
        if (length(object[[1]]$dummy) != 0) {
            l <- length(names(newdata))
            for (i in seq_len(length(object[[1]]$dummy))) {
                newdata[[object[[1]]$dummy[i]]] <- 1
                newdata@.expr[l+i] <- object[[1]]$dummy.expr[i]
            }
        }
        ind.vars <- .replace.col.with.expr1(object[[1]]$ind.vars, newdata)
    } else
        ind.vars <- object[[1]]$ind.vars

    if (object[[1]]$has.intercept && !with.intercept) ind.vars <- c(1, ind.vars)

    if (db$db.str != "HAWQ" || !grepl("^1\\.1", db$version.str)) {
        ind.str <- paste("array[", paste(ind.vars, collapse = ","), "]",
                         sep = "")

    } else {
        if (object[[1]]$has.intercept) ind.vars <- c(1, ind.vars)
    }

    if (length(object) == 1) {
        if (db$db.str != "HAWQ" || !grepl("^1\\.1", db$version.str)) {
            if (object[[1]]$has.intercept && with.intercept) {
                coef <- object[[1]]$coef[-1]
                intercept <- paste(object[[1]]$coef[1], ",", sep = "")
            } else {
                coef <- object[[1]]$coef
                intercept <- if (with.intercept) "0, " else ""
            }
            coef <- paste("array[", paste(coef, collapse = ", "), "]",
                          sep = "")
            expr <- paste(madlib, ".", func.str, "(", coef, ", ", intercept,
                          ind.str, extra, ")", sep = "")
        } else {
            expr <- paste(object[[1]]$coef, ind.vars, sep = "*",
                          collapse = " + ")
            if (func.str == "logregr_predict")
                expr <- paste(expr, " > 0", sep = "")
        }
    } else {
        l <- length(object[[1]]$grp.cols)
        expr <- "case when "
        n <- length(object)
        for (i in seq_len(n)) {
            tmp <- ""
            tmp <- object[[i]]$data@.where
            if (!is(newdata, "db.data.frame"))
                tmp <- .replace.col.with.expr1(gsub(" = ", " == ", tmp),
                                               newdata)
            expr <- paste(expr, tmp, " then ", sep = "")

            if (db$db.str != "HAWQ" || !grepl("^1\\.1", db$version.str)) {
                if (object[[i]]$has.intercept && with.intercept) {
                    coef <- object[[i]]$coef[-1]
                    intercept <- paste(object[[i]]$coef[1], ",", sep = "")
                } else {
                    coef <- object[[i]]$coef
                    intercept <- if (with.intercept) "0, " else ""
                }
                coef.i <- paste("array[", paste(coef, collapse = ", "), "]",
                                sep = "")
                expr <- paste(expr, madlib, ".", func.str, "(", coef.i, ", ",
                              intercept, ind.str, extra, ")", sep = "")
            } else {
                expr <- paste(expr, paste(object[[1]]$coef, ind.vars,
                                          sep = "*", collapse = " + "), sep = "")
                if (func.str == "logregr_predict")
                    expr <- paste(expr, " > 0", sep = "")
            }

            if (i < n)
                expr <- paste(expr, " when ", sep = "")
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
        .factor.ref = as.character(NA),
        .factor.suffix = "",
        .sort = sort,
        .dist.by = newdata@.dist.by)
}

## -----------------------------------------------------------------------

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

## ----------------------------------------------------------------------

.replace.col.with.expr1 <- function(str, data)
{
    vars <- gsub("::[\\w\\s]+", "", str, perl = T)
    vars <- gsub("\"", "`", vars)
    vars <- gsub("\\s", "", vars)
    vars <- .reverse.consistent.func(vars)
    vars <- gsub("\\$\\$", "\"", vars)
    as.vector(sapply(
        vars,
        function(s) {
            r <- eval(parse(text = paste("with(data, ", s, ")", sep = "")))
            r@.expr
        }))
}
