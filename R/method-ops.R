## -----------------------------------------------------------------------
## Some operations for Arith and Compare
## 6 Compare operation methods, 7 Arith operation methods
##
## For each operation, we need to create methods for signatures
## db.data.frame and db.data.frame
## db.Rquery and db.Rquery,
## db.Rquery and numeric, numeric and db.Rquery
## db.Rquery and character, character and db.Rquery
##
## That would be 13 x 6 = 78 methods! How to avoid typing so many
## functions? But of course, they are small functions, and can be
## done in an hour. However, this would bring lots of redundant
## things into the manual.
## -----------------------------------------------------------------------

setGeneric("eql", signature = c("e1", "e2"),
           def = function (e1, e2) standardGeneric("eql"))

## --

## Test whether two object pointing to the same thing
setMethod("eql",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (class(e1)[1] != class(e2)[1])
                  return (FALSE)
              if (is(e1, "db.data.frame")) {
                  if (all(e1@.name == e2@.name) &&
                      e1@.content == e2@.content &&
                      conn.eql(e1@.conn.id, e2@.conn.id) &&
                      e1@.table.type == e2@.table.type)
                      return (TRUE)
                  else
                      return (FALSE)
              } else {
                  if (e1@.content == e2@.content &&
                      length(e1@.expr) == length(e2@.expr) &&
                      all(e1@.expr == e2@.expr) &&
                      e1@.source == e2@.source &&
                      e1@.parent == e2@.parent &&
                      conn.eql(e1@.conn.id, e2@.conn.id) &&
                      all(e1@.col.data_type == e2@.col.data_type) &&
                      .eql.where(e1@.where, e2@.where) &&
                      all(e1@.is.factor == e2@.is.factor) &&
                      all(e1@.col.name == e2@.col.name))
                      return (TRUE)
                  else
                      return (FALSE)
              }
          },
          valueClass = "logical")

## -----------------------------------------------------------------------

## operation between an db.obj and a single value
## @param hide.cast for some operations (*, +, -), cast string is not necessary
.compare <- function(e1, e2, cmp, data.types,
                     prefix = "", res.type = "boolean",
                     cast = "::double precision", res.udt = "bool",
                     restore.array = TRUE, hide.cast = FALSE)
{
    if (is(e1, "db.data.frame")) e1 <- e1[,]

    l <- length(e2)
    count <- 0

    if (is(e1, "db.data.frame"))
        tbl <- content(e1)
    else {
        if (e1@.source == e1@.parent)
            tbl <- e1@.source
        else
            tbl <- paste("(", e1@.parent, ")", sep = "")
    }

    if (e1@.where != "") where.str <- paste(" where", e1@.where)
    else where.str <- ""

    sort <- .generate.sort(e1)

    expr <- rep("", length(names(e1)))
    col.data_type <- rep("", length(names(e1)))
    col.udt_name <- rep("", length(names(e1)))
    ## col.name <- rep("", length(names(e1)))
    col.name <- paste(names(e1), "_opr", sep = "")
    i <- 1
    len <- length(names(e1))
    while (i <= len) {
        col.data_type[i] <- res.type
        col.udt_name[i] <- res.udt
        ## col.name[i] <- paste(names(e1)[i], "_opr", sep = "")
        s <- e2[count %% l + 1]
        whole.array <- is.character(s) && grepl("^\\{", .strip(s)) && grepl("\\}$", .strip(s))
        if (e1@.col.data_type[i] %in% data.types || is.na(data.types) || whole.array) {
            e2.str <- (if (e2[count %% l + 1] == "") "" else paste(
                                                                   "(", e2[count %% l + 1], ")", sep = ""))
            expr[i] <- paste(prefix, "(", e1@.expr[i], ")",
                             (if (hide.cast) "" else cast),
                             cmp, e2.str, sep = "")
            count <- count + 1
            i <- i + 1
        } else if (e1@.col.data_type[i] == "array") {
            tmp <- .get.array.elements(e1@.expr[i], tbl, where.str,
                                       conn.id(e1))
            tn <- length(tmp)
            ## e2.str <- (if (e2[(count + seq(tn) - 1) %% l + 1] == "") ""
            ## else paste("(", e2[count %% l + 1], ")", sep = ""))

            if (cast == "")
                e2.str <- e2[(count + seq(tn) - 1) %% l + 1]
            else
                e2.str <- paste("(", e2[(count + seq(tn) - 1) %% l + 1], ")",
                                sep = "")

            if (restore.array) {
                expr[i] <- paste("array[", paste(prefix, "(", tmp, ")", cast,
                                                 cmp, e2.str, sep = "",
                                                 collapse = ", "), "]",
                                 sep = "")
                col.data_type[i] <- "array"
                col.udt_name[i] <- paste("_", res.udt, sep = "")
                i <- i + 1
            } else {
                expr.t <- paste(prefix, "(", tmp, ")",
                                (if (hide.cast) "" else cast),
                                cmp, e2.str, sep = "")
                expr <- c(expr[seq_len(i-1)], expr.t,
                          expr[(i+1)+seq_len(length(expr)-i)])
                col.data_type <- c(col.data_type[seq_len(i-1)],
                                   rep("boolean", tn),
                                   col.data_type[(i+1)+
                                                 seq_len(length(
                                                                col.data_type)-i)])
                col.udt_name<- c(col.udt_name[seq_len(i-1)],
                                 rep("bool", tn),
                                 col.udt_name[(i+1)+
                                              seq_len(length(
                                                             col.udt_name)-i)])
                col.name <- c(col.name[seq_len(i-1)],
                              paste(col.name[i], "_", seq_len(tn), sep=""),
                              col.name[(i+1)+seq_len(length(col.name)-i)])
                i <- i + tn
                len <- len + tn - 1
            }
            count <- count + tn
        } else {
            expr[i] <- "NULL"
            count <- count + 1
            i <- i + 1
        }
    }

    expr.str <- paste(expr, paste("\"", col.name, "\"", sep = ""),
                      sep = " as ", collapse = ", ")
    new("db.Rquery",
        .content = paste("select ", expr.str, " from ", tbl,
                         where.str, sort$str, sep = ""),
        .expr = expr,
        .source = e1@.source,
        .parent = e1@.parent,
        .conn.id = conn.id(e1),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = e1@.where,
        .is.factor = rep(FALSE, length(col.name)),
        .factor.ref = rep(as.character(NA), length(col.name)),
        .factor.suffix = rep("", length(col.name)),
        .sort = sort,
        .dist.by = e1@.dist.by)
}

## -----------------------------------------------------------------------

setMethod(">",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " > ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("<",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 > e1
          },
          valueClass = "db.Rquery")

## --

setMethod("<",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " < ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod(">",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 < e1
          },
          valueClass = "db.Rquery")

## --

setMethod(">=",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " >= ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("<=",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 >= e1
          },
          valueClass = "db.Rquery")

## --

setMethod("<=",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " <= ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod(">=",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 <= e1
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " = ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 == e1
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "db.obj", e2 = "logical"),
          function (e1, e2) {
              res <- .compare(e1, e2, " = ", "boolean", cast = "")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "logical", e2 = "db.obj"),
          function (e1, e2) {
              e2 == e1
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "db.obj", e2 = "logical"),
          function (e1, e2) {
              res <- .compare(e1, e2, " <> ", "boolean", cast = "")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "logical", e2 = "db.obj"),
          function (e1, e2) {
              e2 != e1
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " <> ", .num.types, hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 != e1
          },
          valueClass = "db.Rquery")

## -----------------------------------------------------------------------

## convert string to time types
.replace.timestamp <- function (e1, res, s, op, res.type.change = FALSE,
                                inverse = FALSE, always.interval = FALSE,
                                is.bool = FALSE)
{
    types <- ifelse(e1@.col.data_type == "array",
                    .strip(e1@.col.udt_name, "_"), e1@.col.udt_name)
    idx <- match(types, .udt.time.types)
    for (i in seq_len(length(types))) {
        if (!is.na(idx[i])) {
            t <- .time.types[idx[i]]
            if (always.interval)
                t1 <- "interval"
            else
                t1 <- t
            if (inverse)
                res@.expr[i] <- paste(s, "::", t1, op, e1@.expr[i], sep = "")
            else
                res@.expr[i] <- paste(e1@.expr[i], op, s, "::", t1, sep = "")
            res@.content <- gsub(paste("NULL as \"", res@.col.name[i],
                                       "\"", sep = ""),
                                 paste(res@.expr[i], " as \"",
                                       res@.col.name[i], "\"", sep = ""),
                                 res@.content)
            if (is.bool) {
                res@.col.data_type[i] <- 'boolean'
                res@.col.udt_name[i] <- 'bool'
            } else if (res.type.change) {
                res@.col.data_type[i] <- .time.change[idx[i]]
                res@.col.udt_name[i] <- .time.change[idx[i]]
            } else {
                res@.col.data_type[i] <- t
                res@.col.udt_name[i] <- t
            }
        }
    }
    res
}

## ----------------------------------------------------------------------

setMethod(">",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " > ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " > ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("<",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 > e1
          },
          valueClass = "db.Rquery")

## --

setMethod("<",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " < ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " < ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod(">",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 < e1
          },
          valueClass = "db.Rquery")

## --

setMethod(">=",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " >= ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " >= ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("<=",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 >= e1
          },
          valueClass = "db.Rquery")

## --

setMethod("<=",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " <= ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " <= ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod(">=",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 <= e1
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " = ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " = ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 == e1
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("$$", .strip(e2, "'"), "$$", sep = "")
              res <- .compare(e1, e2, " <> ", .txt.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " <> ", is.bool = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 != e1
          },
          valueClass = "db.Rquery")

## -----------------------------------------------------------------------

setMethod("&",
          signature(e1 = "db.obj", e2 = "logical"),
          function (e1, e2) {
              res <- .compare(e1, e2, " and ", c("boolean"), res.type = "boolean",
                              cast = "")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("|",
          signature(e1 = "db.obj", e2 = "logical"),
          function (e1, e2) {
              res <- .compare(e1, e2, " or ", c("boolean"), res.type = "boolean",
                              cast = "")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

setMethod("&",
          signature(e1 = "logical", e2 = "db.obj"),
          function (e1, e2) {
              e2 & e1
          },
          valueClass = "db.Rquery")

## --

setMethod("|",
          signature(e1 = "logical", e2 = "db.obj"),
          function (e1, e2) {
              e2 | e1
          },
          valueClass = "db.Rquery")


## -----------------------------------------------------------------------

setMethod("+",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " + ", .num.types,
                              cast = "",
                              res.type = "double precision",
                              res.udt = "float8", hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg

              for (i in seq_len(length(names(res)))) {
                  if (grepl("date$", e1@.col.udt_name[i])) {
                      res@.expr[i] <- paste(e1@.expr, " + ", e2, "::integer",
                                            sep = "")
                      res@.content <- gsub(paste("NULL as \"", res@.col.name[i],
                                                 "\"", sep = ""),
                                           paste(res@.expr[i], " as \"",
                                                 res@.col.name[i], "\"", sep = ""),
                                           res@.content)
                      res@.col.data_type[i] <- "date"
                      res@.col.udt_name[i] <- "date"
                  }
              }
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("+",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 + e1
          },
          valueClass = "db.Rquery")

## --
setMethod("+",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("'", .strip(e2, "'"), "'", sep = "")
              res <- .compare(e1, e2, " + ", .num.types,
                              cast = "",
                              res.type = "double precision",
                              res.udt = "float8")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res <- .replace.timestamp(e1, res, e2, " + ", always.interval = TRUE)
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("+",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e2 + e1
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " - ", .num.types,
                              res.type = "double precision",
                              res.udt = "float8",
                              hide.cast = TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "db.obj", e2 = "character"),
          function (e1, e2) {
              e2 <- paste("'", .strip(e2, "'"), "'", sep = "")
              res <- .compare(e1, e2, " - ", .num.types, cast = "")
              res <- .replace.timestamp(e1, res, e2, " - ", TRUE)
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "character", e2 = "db.obj"),
          function (e1, e2) {
              e1 <- paste("'", .strip(e1, "'"), "'", sep = "")
              res <- .compare(e2, e1, " + ", .num.types, prefix = "-", cast = "")
              res <- .replace.timestamp(e2, res, e1, " - ", TRUE, TRUE)
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "db.obj", e2 = "ANY"),
          function (e1, e2) {
              if (nargs() == 1) e1 * (-1)
              else e1 - e2
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              res <- .compare(e2, e1, " + ", .num.types, prefix = "-",
                              res.type = "double precision", res.udt = "float8",
                              hide.cast = TRUE)
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("*",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              if (length(names(e1)) == 1 && e1@.col.data_type == "array" &&
                  length(e2) == 1) {
                  e1 <- e1[[names(e1)]]
                  res <- e1
                  madlib <- schema.madlib(conn.id(e1))
                  res@.expr <- paste(madlib, ".array_scalar_mult(", e1@.expr,
                                     "::double precision[], ",
                                     e2, "::double precision)", sep = "")
                  res@.col.name <- paste(e1@.col.name, "_opr", sep = "")
                  ## res@.content <- gsub("^select [^((?! as ).)]+\\S+ as",
                  res@.content <- gsub("^select .* as",
                                       paste("select ", res@.expr, " as", sep = ""),
                                       e1@.content)
                  if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
                  return (res)
              } else if (length(names(e1)) == 1 && e1@.col.data_type != "array" &&
                         length(e2) > 1) {
                  res <- e1
                  madlib <- schema.madlib(conn.id(e1))
                  res@.expr <- paste(madlib, ".array_scalar_mult(array[",
                                     paste(e2, collapse=","),
                                     "]::double precision[], ", e1@.expr,
                                     "::double precision)", sep = "")
                  res@.col.name <- paste(e1@.col.name, "_opr", sep = "")
                  ## res@.content <- gsub("^select [^((?! as ).)]+\\S+ as",
                  res@.content <- gsub("^select .* as",
                                       paste("select ", res@.expr, " as", sep = ""),
                                       e1@.content)
                  if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
                  res@.col.data_type <- "array"
                  res@.col.udt_name <- "_float8"
                  return (res)
              } else {
                  res <- .compare(e1, e2, " * ", .num.types,
                                  res.type = "double precision",
                                  res.udt = "float8", hide.cast = TRUE)
                  if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
                  res
              }
          },
          valueClass = "db.Rquery")

## --

setMethod("*",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              e2 * e1
          },
          valueClass = "db.Rquery")

## --

setMethod("/",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " / ", .num.types,
                              res.type = "double precision",
                              cast = "::double precision", res.udt = "float8")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("/",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              res <- .compare(e2, e1, " * ", .num.types, prefix = "1./",
                              res.type = "double precision", res.udt = "float8",
                              hide.cast = TRUE)
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("^",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, "^", .num.types,
                              res.type = "double precision",
                              res.udt = "float8")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("^",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              res <- .compare(e2, "", "", .num.types,
                              prefix = paste(e1, "^", sep = ""),
                              res.type = "double precision", res.udt = "float8")
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("%%",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, e2, " % ", .num.types,
                              res.type = "double precision",
                              cast = "::integer", res.udt = "float8")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("%%",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              res <- .compare(e2, "", "", .num.types,
                              prefix = paste(e1, "% "),
                              res.type = "double precision", cast = "::integer",
                              res.udt = "float8")
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("%/%",
          signature(e1 = "db.obj", e2 = "numeric"),
          function (e1, e2) {
              res <- .compare(e1, as.integer(e2), " / ", .int.types,
                              res.type = "integer", cast = "::integer",
                              res.udt = "int4")
              if (is(e1, "db.Rquery")) res@.is.agg <- e1@.is.agg
              res
          },
          valueClass = "db.Rquery")

## --

setMethod("%/%",
          signature(e1 = "numeric", e2 = "db.obj"),
          function (e1, e2) {
              res <- .compare(e2, "", "", .num.types,
                              prefix = paste(e1, "%"),
                              res.type = "integer", cast = "::integer",
                              res.udt = "int4")
              if (is(e2, "db.Rquery")) res@.is.agg <- e2@.is.agg
              res
          },
          valueClass = "db.Rquery")

## -----------------------------------------------------------------------

## Arith operators for db.Rquery and db.Rquery

## operator for two db.obj objects
.operate.two <- function (e1, e2, op, data.types,
                          res.type = "boolean",
                          cast = "::double precision",
                          res.udt = "bool", hide.cast = FALSE)
{
    ## convert db.data.frame into db.Rquery
    if (is(e1, "db.data.frame")) e1 <- e1[,]
    if (is(e2, "db.data.frame")) e2 <- e2[,]

    if (! conn.eql(conn.id(e1), conn.id(e2)))
        stop("The two objects are not in the same database!")
    if (!.eql.parent(e1, e2))
        stop("x and y cannot match because they originate",
             " from different sources!")
    conn.id <- conn.id(e1)

    if (e1@.source == e1@.parent)
        tbl <- e1@.source
    else
        tbl <- paste("(", e1@.parent, ")", sep = "")

    if (e1@.where != "") where.str <- paste(" where", e1@.where)
    else where.str <- ""

    sort <- .generate.sort(e1)

    l1 <- length(names(e1))
    l2 <- length(names(e2))
    if (l1 > l2)
        l <- l1
    else
        l <- l2

    if (e1@.parent != e2@.parent || !.eql.where(e1@.where, e2@.where))
        stop("How can you match the rows of two objects",
             " if they are not derived from the same thing!")

    expr <- rep("", length(names(e1)))
    col.data_type <- rep("", length(names(e1)))
    col.udt_name <- rep("", length(names(e1)))
    col.name <- rep("", length(names(e1)))
    for (i in seq_len(l)) {
        i1 <- (i-1) %% l1 + 1
        i2 <- (i-1) %% l2 + 1
        v <- 0
        if (e1@.col.data_type[i1] == "array") {
            tmp1 <- .get.array.elements(e1@.expr[i1], tbl, where.str,
                                        conn.id)
            if (e2@.col.data_type[i2] == "array") {
                tmp2 <- .get.array.elements(e2@.expr[i2], tbl, where.str,
                                            conn.id)
                if (length(tmp2) != length(tmp1) && length(tmp2) != 1)
                    stop("Two arrays have to have the same length or one ",
                         "of them has length of 1!")
                expr[i] <- paste("array[", paste("(", tmp1, ")",
                                                 (if (hide.cast) "" else cast),
                                                 op, "(",
                                                 tmp2, ")", sep = "",
                                                 collapse = ", "),
                                 "]", sep = "")
            } else {
                expr[i] <- paste("array[", paste("(", tmp1, ")",
                                                 (if (hide.cast) "" else cast),
                                                 op, "(",
                                                 e2@.expr[i2], ")",
                                                 sep = "",
                                                 collapse = ", "),
                                 "]", sep = "")
            }
            col.data_type[i] <- res.type
            col.udt_name[i] <- res.udt
            col.name[i] <- paste(names(e1)[i1], "_", names(e2)[i2],
                                 "_opr", sep = "")
            break
        } else if (e2@.col.data_type[i2] == "array") {
            tmp2 <- .get.array.elements(e2@.expr[i2], tbl, where.str,
                                        conn.id)
            expr[i] <- paste("array[", paste("(", e1@.expr[i1], ")",
                                             (if (hide.cast) "" else cast),
                                             op, "(",
                                             tmp2, ")",
                                             sep = "",
                                             collapse = ", "),
                             "]", sep = "")
            break
        }

        for (k in seq_len(length(data.types)))
            if (e1@.col.data_type[i1] %in% data.types[[k]]) {
                v <- k
                break
            }
        if (v > 0 && e2@.col.data_type[i2] %in% data.types[[v]]) {
            expr[i] <- paste("(", e1@.expr[i1], ")", (if (hide.cast) "" else cast),
                             op, "(", e2@.expr[i2], ")", sep = "")
        } else {
            expr[i] <- "NULL"
        }
        col.data_type[i] <- res.type
        col.udt_name[i] <- res.udt
        col.name[i] <- paste(names(e1)[i1], "_", names(e2)[i2],
                             "_opr", sep = "")
    }

    expr.str <- paste(expr, paste("\"", col.name, "\"", sep = ""),
                      sep = " as ", collapse = ", ")

    new("db.Rquery",
        .content = paste("select ", expr.str, " from ", tbl,
                         where.str, sort$str, sep = ""),
        .expr = expr,
        .source = e1@.source,
        .parent = e1@.parent,
        .conn.id = conn.id(e1),
        .col.name = col.name,
        .key = character(0),
        .col.data_type = col.data_type,
        .col.udt_name = col.udt_name,
        .where = e1@.where,
        .is.factor = rep(FALSE, l),
        .factor.ref = rep(as.character(NA), l),
        .factor.suffix = rep("", l),
        .sort = sort,
        .dist.by = e1@.dist.by)
}

## -----------------------------------------------------------------------

setMethod("+",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 + e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 + e2)
              }
              .operate.two(e1, e2, " + ", list(.num.types),
                           res.type = "double precision",
                           res.udt = "float8", hide.cast = TRUE)
          },
          valueClass = "db.Rquery")

## --

setMethod("-",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 - e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 - e2)
              }

              .combine.list(sapply(
                                   seq_len(length(names(e1))),
                                   function(i) {
                                       s1 <- sapply(.udt.time.types,
                                                    function(s) grepl(paste(s, "$", sep = ""),
                                                                      e1@.col.udt_name[i]))
                                       s2 <- sapply(.udt.time.types,
                                                    function(s) grepl(paste(s, "$", sep = ""),
                                                                      e2@.col.udt_name[i]))

                                       if (any(s1) || any(s2)) {
                                           id <- union(seq_len(length(.time.types))[s1],
                                                       seq_len(length(.time.types))[s2])[1]
                                           udt <- (if (.time.change[id] == "integer")
                                                   "int4"
                                               else .time.change[id])

                                           .operate.two(
                                                        e1[[i]], e2[[i]], " - ", list(.time.types),
                                                        res.type = .time.change[id],
                                                        cast = '',
                                                        res.udt = (if (e1@.col.data_type[i] == "array")
                                                                   paste("_", udt, sep = "") else udt))
                                       } else {
                                           .operate.two(e1[[i]], e2[[i]], " - ", list(.num.types),
                                                        res.type = "double precision",
                                                        res.udt = "float8", hide.cast = TRUE)
                                       }
                                   }))
          },
          valueClass = "db.Rquery")

## --

setMethod("*",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 * e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 * e2)
              }
              if (length(names(e1)) == 1 && e1@.col.data_type == "array" &&
                  length(names(e2)) == 1 && e2@.col.data_type %in% .num.types) {
                  if (! conn.eql(conn.id(e1), conn.id(e2)))
                      stop("The two objects are not in the same database!")
                  if (!.eql.parent(e1, e2))
                      stop("x and y cannot match because they originate",
                           " from different sources!")
                  e1 <- e1[[names(e1)]]
                  e2 <- e2[,]
                  res <- e1
                  madlib <- schema.madlib(conn.id(e1))
                  res@.expr <- paste(madlib, ".array_scalar_mult(", e1@.expr,
                                     "::double precision[], (",
                                     e2@.expr, ")::double precision)", sep = "")
                  res@.col.name <- paste(e1@.col.name, "_opr", sep = "")
                  ## res@.content <- gsub("^select [^((?! as ).)]+\\S+ as",
                  res@.content <- gsub("^select .* as",
                                       paste("select ", res@.expr, " as", sep = ""),
                                       e1@.content)
                  return (res)
              } else if (length(names(e2)) == 1 && e2@.col.data_type == "array" &&
                         length(names(e1)) == 1 &&
                         e1@.col.data_type %in% .num.types) {
                  return (e2 * e1)
              } else {
                  res <- .operate.two(e1, e2, " * ", list(.num.types),
                                      res.type = "double precision",
                                      res.udt = "float8", hide.cast = TRUE)
                  if (length(names(e1)) == 1 && e1@.col.data_type == "array" &&
                      length(names(e2)) == 1 && e2@.col.data_type == "array") {
                      res@.col.data_type <- "array"
                      res@.col.udt_name <- "_float8"
                  }
                  res
              }
          },
          valueClass = "db.Rquery")

## --

setMethod("/",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 / e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 / e2)
              }
              .operate.two(e1, e2, " / ", list(.num.types),
                           res.type = "double precision",
                           res.udt = "float8")
          },
          valueClass = "db.Rquery")

## --

setMethod("^",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1^e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1^e2)
              }
              .operate.two(e1, e2, " ^ ", list(.num.types),
                           res.type = "double precision",
                           res.udt = "float8")
          },
          valueClass = "db.Rquery")

## --

setMethod("%%",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 %% e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 %% e2)
              }
              .operate.two(e1, e2, " % ", list(.num.types),
                           res.type = "double precision",
                           cast = "::integer",
                           res.udt = "float8")
          },
          valueClass = "db.Rquery")

## --

setMethod("%/%",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 %/% e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 %/% e2)
              }
              .operate.two(e1, e2, " / ", list(.int.types),
                           res.type = "integer",
                           cast = "::integer",
                           res.udt = "int4")
          },
          valueClass = "db.Rquery")

## --

setMethod(">",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 > e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 > e2)
              }
              .operate.two(e1, e2, " > ", list(.num.types, .txt.types),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("<",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 < e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 < e2)
              }
              .operate.two(e1, e2, " < ", list(.num.types, .txt.types),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod(">=",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 >= e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 >= e2)
              }
              .operate.two(e1, e2, " >= ", list(.num.types, .txt.types),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("<=",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 <= e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 <= e2)
              }
              .operate.two(e1, e2, " <= ", list(.num.types, .txt.types),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("==",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 == e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 == e2)
              }
              .operate.two(e1, e2, " = ", list(.num.types, .txt.types, "boolean"),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("!=",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 != e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 != e2)
              }
              .operate.two(e1, e2, " <> ", list(.num.types, .txt.types, "boolean"),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("&",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 & e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 & e2)
              }
              .operate.two(e1, e2, " and ", list(c("boolean")),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("|",
          signature(e1 = "db.obj", e2 = "db.obj"),
          function (e1, e2) {
              if (is(e2, "db.Rquery") && e2@.is.agg) {
                  e2 <- unlist(lk(e2))
                  return (e1 | e2)
              }
              if (is(e1, "db.Rquery") && e1@.is.agg) {
                  e1 <- unlist(lk(e1))
                  return (e1 | e2)
              }
              .operate.two(e1, e2, " or ", list(c("boolean")),
                           res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## --

setMethod("!",
          signature(x = "db.obj"),
          function (x) {
              .compare(x, "", "", c("boolean"), prefix = "not ",
                       res.type = "boolean", cast = "")
          },
          valueClass = "db.Rquery")

## -----------------------------------------------------------------------

setGeneric("is.na")

setMethod("is.na",
          signature(x = "db.obj"),
          function (x) {
              ## if (array)
              ##     .compare(x, "", " is NULL", NA, "", "boolean", "",
              ##              restore.array = FALSE)
              ## else
              res <- .compare(x, "", " is NULL", NA, "", "boolean", "")
              if (length(x@.col.data_type) == 1 && x@.col.data_type == "array") {
                  .check.madlib.version(x, 1.3)
                  madlib <- schema.madlib(conn.id(x))
                  tmp <- paste(res@.expr, " or ", madlib,
                               ".array_contains_null(", x@.expr, ")", sep="")
                  ## res@.content <- gsub("^select [^((?! as ).)]+\\S+ as",
                  res@.content <- gsub("^select .* as",
                                       paste("select", tmp, "as"),
                                       res@.content)
                  res@.expr <- tmp
              }
              res
          },
          valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setGeneric("grepl")

setMethod("grepl",
          signature(pattern = "character", x = "db.obj"),
          function (pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                    useBytes = FALSE)
          {
              pattern <- paste("'", .strip(as.character(pattern), "'"), "'", sep = "")
              if (fixed) pattern <- gsub("\\", "\\\\", pattern)
              if (ignore.case) cmp <- "~*"
              else cmp <- "~"
              res <- .compare(x, pattern, cmp, .txt.types, cast = "")
              res
          })

## ----------------------------------------------------------------------

`%+%` <- function (e1, e2) paste(e1, e2, sep = "")
`%.%` <- function (e1, e2) paste(e2, collapse = e1)
