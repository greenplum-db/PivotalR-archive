## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## -----------------------------------------------------------------------

## convert {...} string into an arrayparams$grp.str
arraydb.to.arrayr <- function (str, type = "double", n = 1)
{
    if (is.null(str)) return (NULL)

    if (type == "character")
        res <- character(0)
    else if (type == "integer")
        res <- integer(0)
    else if (type == "logical")
        res <- logical(0)
    else
        res <- numeric(0)

    if (type == "character") {
        elm <- matrix(unlist(.regmatches(
            str,
            gregexpr("[^,\"\\s\\{\\}]+|\"[^\"]*\"",
                     str, perl=T))), nrow = length(str), byrow = TRUE)
    } else {
        elm <- matrix(unlist(strsplit(gsub("(\\{|\\})", "", str), ",")),
                      nrow = length(str), byrow = TRUE)
        elm[elm == "NULL"] <- NA
        if (type == "integer")
            class(elm) <- "integer"
        else if (type == "logical") {
            elm <- toupper(elm)
            class(elm) <- "logical"
        } else
            class(elm) <- "numeric"
    }

    elm
}

## -----------------------------------------------------------------------

.is.arg.string <- function (arg)
{
    return (!is.null(arg) &&
            is.character(arg))
}

## -----------------------------------------------------------------------

.is.conn.id.valid <- function (conn.id)
{
    return (length(conn.id) != 0 &&
            !is.null(conn.id) &&
            !is.na(conn.id) &&
            is(conn.id, "numeric") &&
            conn.id >= 1 &&
            length(.localVars$conn.id) != 0 &&
            conn.id %in% .localVars$conn.id[,1])
}

## -----------------------------------------------------------------------

## get the distributed by string
.get.distributed.by.str <- function(conn.id, distributed.by)
{
    dbms.str <- (.get.dbms.str(conn.id))$db.str
    if (dbms.str != "PostgreSQL") {
        if (is.null(distributed.by) ||
            identical(distributed.by, character(0)))
            dist.str <- "DISTRIBUTED RANDOMLY"
        else {
            if (!.is.arg.string(distributed.by))
                stop("distributed.by must be a string or NULL!")
            if (distributed.by == "") # "" means no distributed by
                dist.str <- ""
            else
                dist.str <- paste("DISTRIBUTED BY (",
                                  paste("\"", distributed.by, "\"", sep = ""),
                                  ")", sep = "")
        }
    } else
        dist.str <- ""
    return (dist.str)
}

## -----------------------------------------------------------------------

## simply return the most direct answer
.db.analyze.table.name <- function (name)
{
    parts <- strsplit(name, "\\.")[[1]]
    l <- length(parts)
    if (l != 1 && l != 2)
        stop("The database object name is not valid!")
    return (parts)
}

## ----------------------------------------------------------------------

.db.table.schema.str <- function (table, conn.id)
{
    warns <- .suppress.warnings(conn.id)
    table <- .strip(table, "\"")
    l <- length(table)
    if (l == 2) {
        .restore.warnings(warns)
        return (paste("table_name = '", table[2],
                      "' and table_schema = '",
                      table[1], "'", sep = ""))
    } else if (l == 1) {
        schemas <- arraydb.to.arrayr(
            db.q("select current_schemas(True)",
                 conn.id=conn.id, verbose = FALSE),
            type = "character")
        table_schema <- character(0)
        for (schema in schemas)
            if (.db.existsTable(c(schema, table), conn.id)) {
                table_schema <- c(table_schema, schema)
                break
            }
        .restore.warnings(warns)
        if (identical(table_schema, character(0))) {
            stop("This table does not exist in the search path!")
        } else {
            return (paste("table_name = '", table,
                          "' and table_schema = '", table_schema,
                          "'", sep = ""))
        }
    }
}

## -----------------------------------------------------------------------

## Is the object in database a table?
.is.table.or.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(
        paste("select count(*) from information_schema.tables where ",
              .db.table.schema.str(table, conn.id), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## -----------------------------------------------------------------------

## Is the object in database a view?
.is.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(
        paste("select count(*) from information_schema.views where ",
              .db.table.schema.str(table, conn.id), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## -----------------------------------------------------------------------

.unique.string <- function ()
{
    hex_digits <- c(as.character(0:9), letters[1:6])
    y_digits <- hex_digits[9:12]

    s <- paste(
        paste(sample(hex_digits, 8), collapse=''),
        paste(sample(hex_digits, 4), collapse=''),
        ## paste('4', sample(hex_digits, 3), collapse=''),
        paste(sample(y_digits,1), sample(hex_digits, 3),
              collapse='', sep = ''),
        paste(sample(hex_digits, 12), collapse=''), sep='_')
    s <- paste("madlib_temp_", s, sep = "")
    s
}

.unique.pattern <- function()
    "madlib_temp_[a-f\\d]{8}_[a-f\\d]{4}_[a-f\\d]{6}_[a-f\\d]{12}"

.unique.string.short <- function ()
{
    hex_digits <- c(as.character(0:9), letters[1:6])
    s <- paste(sample(hex_digits, 4), collapse='')
    paste("_mad", s, "_", sep = "")
}

.unique.pattern.short <- function()
    "_mad[a-f\\d]{4}_"

## -----------------------------------------------------------------------

## strip the leading and trailing white spaces
.strip <- function (str, rm = "\\s")
{
    rm.str <- paste("^", rm, "*(.*[^", rm, "])", rm, "*$", sep = "")
    gsub(rm.str, "\\1", str, perl = TRUE)
}

## -----------------------------------------------------------------------

## analyze formula
.analyze.formula <- function (formula, data, fdata = data, refresh = FALSE,
                              is.factor = NA, cols = NA, suffix = NA,
                              grp.vars = NULL, grp.expr = NULL, no.factor = FALSE)
{
    f.str <- strsplit(paste(deparse(formula), collapse = ""), "\\|")[[1]]
    ## f.str <- .replace.array(f.str, data)
    fstr <- f.str[1]
    fstr <- gsub("as\\.factor\\((((?!as\\.factor).)*?)\\)", "factor(\\1)",
                 fstr, perl = T)
    fstr <- gsub("([^\\.]|^)factor\\((((?!as\\.factor).)*?)\\)", "\\1as.factor(\\2)",
                 fstr, perl = T)

    f2 <- f.str[2] # grouping columns, might be NA

    ## fdata <- .expand.array(fdata)
    if (!is.null(grp.vars) && !is.na(f2)) {
        f2.labels <- grp.vars
        grp.expr <- grp.expr
        grp <- paste(f2.labels, collapse = ", ")
    } else if (!is.na(f2)) {
        f2.terms <- terms(formula(paste("~", f2)))
        f2.labels <- attr(f2.terms, "term.labels")

        ## ## grouping column do not use factor
        f2.labels <- gsub("I\\((.*?)\\)", "(\\1)", f2.labels, perl = T)
        f2.labels <- gsub("as\\.factor\\((.*?)\\)", "(\\1)", f2.labels, perl = T)
        f2.labels <- gsub("factor\\((.*?)\\)", "(\\1)", f2.labels, perl = T)

        grp.expr <- f2.labels
        for (i in seq_len(length(f2.labels))) {
            if (! f2.labels[i] %in% names(data)) {
                grp.col <- .unique.string()
                data[[grp.col]] <- eval(parse(text = paste("with(data, ",
                                              f2.labels[i], ")", sep = "")))
                f2.labels[i] <- grp.col
            }
        }
        grp <- paste(f2.labels, collapse = ", ")
    } else {
        f2.labels <- NULL
        grp <- NULL
    }

    if (!is.null(f2.labels))
        fstr <- paste(fstr, "-", paste("`", f2.labels, "`",
                                       collapse = "-", sep = ""))

    ## create a fake data.frame only to extract
    ## terms when there is "." in formula
    fake.data <- structure(vector("list", ncol(fdata)), names=names(fdata), class="data.frame")
    f1 <- formula(fstr) # formula
    f.terms <- terms(f1, data = fake.data) # formula terms
    ## the 1st row is the dependent variable
    f.factors <- attr(f.terms, "factors")
    f.labels <- attr(f.terms, "term.labels") # each terms on the right side
    ## f.labels <- gsub("`([^`]*)(\\[\\d+\\])`", "\"\\1\"\\2", f.labels)
    right.hand <- paste(f.labels, collapse = "+")
    if (refresh) { # second pass
        right.hand <- gsub("as\\.factor\\((((?!as\\.factor).)*?)\\)", "(\\1)", right.hand, perl = T)
        if (sum(data@.is.factor) > 0 && !no.factor) {
            distinct <- list()
            ref <- list()
            replace.cols <- cols[is.factor]
            suffix <- suffix[is.factor]
            the.refs <- data@.factor.ref[is.factor]
            max.level <- 0
            for (i in seq_len(length(replace.cols))) {
                col <- replace.cols[i]
                new.col <- gsub(paste("^", col, sep = ""), "",
                                names(data)[grep(paste(col, suffix[i],
                                                       sep=""),
                                                 names(data))],
                                perl = TRUE)
                distinct[[col]] <- new.col
                ref[[col]] <- paste("`", suffix[i], the.refs[i], "`", sep = "")
                if (length(new.col) > max.level)
                    max.level <- length(new.col)
            }

            right.hand1 <- right.hand
            for (col in names(data)) {
                right.hand1 <- gsub(paste("(", col, "\\s*\\[.*\\])", sep=""),
                                    "`\\1`", right.hand1, perl = T)
            }
            vars <- all.vars(parse(text = right.hand1))
            vars <- setdiff(vars, names(data))
            l <- length(names(data))
            fake <- as.data.frame(array(1, dim = c(max.level, l + length(vars))))
            names(fake) <- c(names(data), vars)
            for (i in seq_len(l)) {
                if (data@.is.factor[i]) {
                    fake[,i] <- array(paste("`", distinct[[data@.col.name[i]]], "`",
                                            sep = ""), dim = c(max.level, 1))
                    fake[,i] <- as.factor(fake[,i])
                    fake[,i] <- relevel(fake[,i],
                                        ref = ref[[data@.col.name[i]]])
                }
            }
            if (attr(f.terms, "intercept") == 0) inter.str <- "-1"
            else inter.str <- ""
            fterm <- .modeling.formula(formula(paste("~", right.hand1, inter.str)), fake)
            if (fterm[1] == "(Intercept)") fterm <- fterm[-1]
            right.hand <- paste(fterm, collapse = " + ")
        }
    } else { # first pass
        ## find all the factor columns
        ## right.hand <- gsub("as.factor\\s*\\((.*)\\)",
        ##                    "factor(\\1)", right.hand, perl = T)
        elm <- .regmatches(right.hand,
                           gregexpr("as\\.factor\\s*\\([^\\(\\)]+\\)",
                                    right.hand, perl=T))[[1]]
        col <- .strip(gsub("as\\.factor\\s*\\(([^\\(\\)]+?)\\)", "(\\1)", elm,
                           perl = T))
        col <- gsub("^\\((.*?)\\)$", "\\1", col)
        if (!all(col %in% names(data)))
            stop("At least one of the variables cannot be set to be a factor ",
                 "because either it does not exist in the data table ",
                 "or it is an element of an array!")

        ## make sure the returned object is always db.Rquery
        for (i in seq_len(length(data@.col.name)))
            if (data@.is.factor[i]) data[[i]] <- as.factor(data[[i]])

        ## factors added by formula
        for (cl in col) data[[cl]] <- as.factor(data[[cl]])

        right.hand <- gsub("as\\.factor\\((((?!as\\.factor).)*?)\\)", "(\\1)", right.hand, perl = T)
        ## right.hand <- gsub("factor\\((((?!factor).)*)\\)", "\\1", right.hand, perl = T)
    }

    f.terms1 <- terms(formula(paste("~", right.hand)), data = fake.data)
    f.labels <- attr(f.terms1, "term.labels")
    ## f.labels <- gsub("`([^`]*)(\\[\\d+\\])`", "\"\\1\"\\2", f.labels)
    f.intercept <- attr(f.terms, "intercept")
    ## labels <- .gsub("\\[(\\d+):(\\d+)\\]", "[\\1@\\2]", f.labels)
    ## labels <- gsub(":", "*", labels, perl = T) # replace interaction : with *
    ## labels <- .gsub("\\[(\\d+)@(\\d+)\\]", "[\\1:\\2]", labels)
    labels <- .replace.colon(f.labels)
    labels <- gsub("I\\((.*?)\\)", "(\\1)", labels, perl = T) # remove I()

    ## vdata <- .expand.array(data)
    vdata <- data

    ## dependent variable
    ## factor does not play a role in dependent variable
    dep.var <- gsub("I\\((.*?)\\)", "(\\1)", rownames(f.factors)[1], perl = T)
    dep.var <- gsub("as\\.factor\\((.*?)\\)", "(\\1)", dep.var, perl = T)
    dep.var <- gsub("factor\\((.*?)\\)", "(\\1)", dep.var, perl = T)
    ## dep.var <- .replace.with.quotes(dep.var, data@.col.name)
    origin.dep <- dep.var

    tmp <- eval(parse(text = paste("with(vdata, ", dep.var, ")", sep = "")))
    dep.var <- tmp@.expr

    ## dep.var <- gsub("`([^`]*)(\\[\\d+\\])`", "\"\\1\"\\2", dep.var)

    ## with or without intercept
    if (f.intercept == 0)
        intercept.str <- ""
    else
        intercept.str <- "1,"

    labels <- gsub("`([^`]*\\[.*\\])`", "\\1", labels, perl = T)
    labels <- .is.array(labels, data)
    orig.labels <- labels

    a <- eval(parse(text = paste("with(vdata, c(",
                    paste(labels, collapse = ", "), "))",
                    sep = "")))
    a.labels <- unlist(sapply(a, function(x) x[,]@.expr))

    ## b <- eval(parse(text = paste("with(vdata, c(",
    ##                 paste(.gsub("I\\((.*)\\)", "(\\1)",
    ##                            setdiff(rownames(f.factors),
    ##                                    colnames(f.factors))),
    ##                       collapse = ", "), "))", sep = "")))
    b <- eval(parse(text = paste("with(vdata, c(",
        paste(.replace.colon(gsub("I\\((.*?)\\)", "(\\1)",
            rownames(f.factors)[rowSums(f.factors) == 0])),
              collapse = ", "), "))", sep = "")))
    ## b2 <- eval(parse(text = paste("with(vdata, c(",
    ##     paste(.replace.colon(.gsub("I\\((.*)\\)", "(\\1)", colnames(f.factors))),
    ##           collapse = ", "), "))", sep = "")))
    b.labels <- unlist(sapply(b, function(x) x[,]@.expr))
    ## b2.labels <- unlist(sapply(b2, function(x) x[,]@.expr))
    ## remove <- match(b2.labels, b1.labels)
    ## remove <- remove[!is.na(remove)]
    ## if (length(remove) >0) b.labels <- b1.labels[-remove]
    ## else b.labels <- character(0)
    ## b.labels <- b1.labels[rowSums(f.factors) == 0]
    labels <- .strip(setdiff(a.labels, b.labels), "`")

    ## labels <- .replace.with.quotes(labels, data@.col.name)
    ## remove grouping columns, when there is no intercept term
    if (!is.null(f2.labels) && f.intercept != 0)
        labels <- setdiff(labels, .strip(f2.labels, "`"))

    ind.var <- paste("array[", intercept.str,
                     paste(labels, collapse = ","),
                     "]", sep = "") # independent variable

    dep.var <- .consistent.func(dep.var)
    ind.var <- .consistent.func(ind.var)
    labels <- .consistent.func(labels)

    dep.var <- gsub("`", "", dep.var)
    ind.var <- gsub("`", "", ind.var)
    if (!is.null(grp)) grp <- gsub("`", "", grp)
    labels <- gsub("`", "", labels)

    factor.full <- rep(TRUE, length(names(data)))
    if (!refresh) {
        model.vars <- .prepare.ind.vars(orig.labels)
        model.vars <- gsub("`\"([^\\[\\]]*?)\"\\[(\\d+?)\\]`", "`\\1[\\2]`", model.vars)
        vars <- unique(all.vars(parse(text = model.vars)))
        idx <- match(vars, names(data))
        for (i in seq_len(length(idx))) {
            id <- idx[i]
            if (!is.na(id)) {
                if (data@.col.data_type[id] %in% c("boolean", .txt.types) &&
                    !data@.is.factor[id]) {
                    data[[vars[i]]] <- as.factor(data[[vars[i]]])
                }
            }
        }
    }

    list(dep.str = dep.var, origin.dep = origin.dep,
         origin.ind = orig.labels,
         ind.str = ind.var,
         grp.str = grp, grp.vars = f2.labels, grp.expr = grp.expr,
         ind.vars = labels,
         has.intercept = as.logical(f.intercept),
         data = data,
         factor.full = factor.full,
         terms = f.terms)
}

## -----------------------------------------------------------------------

## repeatedly replace all patterns
.gsub <- function(pattern, replace, str, ...)
{
    while (TRUE) {
        .str <- gsub(pattern, replace, str, ...)
        if (identical(.str, str)) return (.str)
        str <- .str
    }
}

## ----------------------------------------------------------------------

.replace.colon <- function(s)
{
    r <- gsub("(\\d+)\\s*:\\s*(\\d+)", "\\1@\\2", s)
    r <- gsub(":", "*", r, perl = T)
    r <- gsub("(\\d+)@(\\d+)", "\\1:\\2", r)
    r
}

## ----------------------------------------------------------------------

.modeling.formula <- function(formula, data)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    x <- model.matrix(mt, mf, contrasts)
    res <- colnames(x)
    for (s in names(data)) {
        if (all(!grepl(paste("`", s, "`", sep = ""), res, fixed = TRUE))) {
            res <- gsub(paste(s, "`(.+)$", sep = ""),
                        paste("`", s, "\\1", sep = ""),
                        res, perl = T)
        }
    }
    res
}

## ----------------------------------------------------------------------

## R's log is SQL's log(exp(1.), x)
## R's log10 is SQL's log
.consistent.func <- function (strs)
{
    res <- gsub("log\\s*\\(", "ln(", strs)
    res <- gsub("log10\\s*\\(", "log(", res)
    ## res <- gsub("log2\\s*\\(", "log(2.,", res)
    ## res <- gsub("logb\\s*\\(()", "log(", res)
    res
}

## ----------------------------------------------------------------------

## reverse of the above function
.reverse.consistent.func <- function (strs)
{
    res <- gsub("log\\s*\\(", "log10(", strs)
    res <- gsub("ln\\s*\\(", "log(", res)
    res
}

## -----------------------------------------------------------------------

.replace.with.quotes <- function (vars, cols)
{
    n.order <- order(nchar(cols), decreasing = TRUE)
    cols <- cols[n.order]
    for (i in seq_len(length(cols))) {
        col <- cols[i]
        vars <- gsub(paste(col, "([^_\\w]+?)", sep = ""),
                     paste("\"", col, "\"\\1", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("([_\\w]+?)\"", col, "\"", sep = ""),
                     paste("\\1", col, sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("([^_\\w\"]+?)", col, sep = ""),
                     paste("\\1\"", col, "\"", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("\"", col, "\"([_\\w]+?)", sep = ""),
                     paste(col, "\\1", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("^", col, "$", sep = ""),
                     paste("\"", col, "\"", sep = ""),
                     vars, perl = TRUE)
    }
    ## vars <- gsub("([\\w_]+)", "\"\\1\"", vars, perl = T)
    vars
}

## -----------------------------------------------------------------------

## interactively read an input from the user
.read.input <- function (msg, expected.answer = c("yes", "y", "no", "n"))
{
    a <- tolower(readline(msg))
    while (! a %in% expected.answer)
        a <- tolower(readline(msg))
    a
}

## -----------------------------------------------------------------------

## set/reset the message level
## returns the old message level
.set.msg.level <- function (level, conn.id)
{
    old.level <- .db.getQuery("select setting from pg_settings where name='client_min_messages'", conn.id)
    .db.getQuery(paste("set client_min_messages to", level), conn.id)
    old.level
}

## ----------------------------------------------------------------------

.db.data.frame2db.Rquery <- function(x)
{
    if (is(x, "db.data.frame")) {
        if (length(names(x)) == 1 && x@.col.data_type == "array") {
            if (array)
                x <- db.array(x)
            else
                x <- x[[names(x)]]
        } else
            x <- x[,]
    }
    x
}

## -----------------------------------------------------------------------

## If an independent variable is an array, it needs special treatment
.is.array <- function (labels, data)
{
    nlabels <- character(0)
    data <- .db.data.frame2db.Rquery(data)
    if (data@.parent == data@.source)
        tbl <- data@.parent
    else
        tbl <- paste("(", data@.parent, ") s", sep = "")
    if (data@.where != "") where.str <- paste(" where", data@.where)
    else where.str <- ""
    conn.id <- conn.id(data)

    for (i in seq_len(length(labels))) {
        if (labels[i] %in% names(data) &&
            data@.col.data_type[which(names(data) == labels[i])] == "array") {
            n <- .db("select array_upper(\"", labels[i],
                     "\",1) from ",
                     tbl, where.str, " limit 1", sep = "",
                     conn.id = conn.id, verbose = F)[[1]]
            n0 <- .db("select array_lower(\"", labels[i],
                      "\",1) from ",
                      tbl, where.str, " limit 1", sep = "",
                      conn.id = conn.id, verbose = F)[[1]]
            nlabels <- c(nlabels, paste(labels[i], "[",
                                        seq_len(n-n0+1) -1 + n0,
                                        "]", sep = ""))
        } else {
            nlabels <- c(nlabels, labels[i])
        }
    }
    nlabels
}

## -----------------------------------------------------------------------

## replace array in the formula
.replace.array <- function (fstr, data)
{
    data <- data[,]
    if (data@.parent == data@.source)
        tbl <- data@.parent
    else
        tbl <- paste("(", data@.parent, ") s", sep = "")
    if (data@.where != "") where.str <- paste(" where", data@.where)
    else where.str <- ""
    conn.id <- conn.id(data)

    for (i in seq_len(length(data@.col.name))) {
        if (data@.col.data_type[i] == "array") {
            n <- .db.getQuery(paste("select array_upper(\"",
                                    data@.col.name[i], "\",1) from ",
                                    tbl, where.str, " limit 1", sep = ""),
                              conn.id)[[1]]
            n0 <- .db.getQuery(paste("select array_lower(\"",
                                     data@.col.name[i],
                                     "\",1) from ",
                                     tbl, where.str, " limit 1", sep = ""),
                              conn.id)[[1]]
            l <- n - n0 + 1
            s <- paste("`", data@.col.name[i], "[", seq_len(l) - 1 + n0,
                       "]`",
                       sep = "", collapse = " + ")
            fstr <- gsub(paste(data@.col.name[i], "\\s*([^\\[]|$)",
                               sep = ""),
                         paste("(", s, ")\\1", sep = ""), fstr)
        }
    }
    fstr
}

## -----------------------------------------------------------------------

## whether two where strings are equivalent
.eql.where <- function (where1, where2)
{
    s <- gsub("^not \\((.*?)\\)$", "\\1", where1, perl = TRUE)
    t <- gsub("^not \\((.*?)\\)$", "\\1", where2, perl = TRUE)
    if (s != where1 && t != where2) {
        return (.eql.where(s, t))
    } else if ((s != where1 && t == where2) ||
               (s == where1 && t != where2)) {
        return (FALSE)
    }

    s1 <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\1", s, perl = TRUE)
    if (s1 == s) return (s == t)
    s2 <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\3", s, perl = TRUE)
    ss <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\2", s, perl = TRUE)

    t1 <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\1", t, perl = TRUE)
    if (t1 == t) return (FALSE)
    t2 <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\3", t, perl = TRUE)
    tt <- gsub("^\\((.*?)\\) (and|or) \\((.*?)\\)$", "\\2", t, perl = TRUE)

    if ((.eql.where(s1, t1) && .eql.where(s2, t2) && ss == tt) ||
        (.eql.where(s1, t2) && .eql.where(s2, t1) && ss == tt))
        TRUE
    else
        FALSE
}
