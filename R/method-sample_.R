
## -----------------------------------------------------------------------
## Sample method with or without replace
## -----------------------------------------------------------------------

setGeneric("sample")

## For now, prob is not implemented
setMethod (
    "sample",
    signature(x = "db.obj"),
    function (x, size, replace = FALSE, prob = NULL, ...) {
        n <- dim(x)[1]
        if (!replace && n < size)
            stop("size is larger than data size!")

        warnings <- .suppress.warnings(conn.id(x))

        if (!replace) {
            tmp <- .unique.string()
            res <- as.db.data.frame(sort(x, FALSE, "random"), tmp, FALSE,
                                    FALSE, TRUE, FALSE, NULL, size)
            .restore.warnings(warnings)
            res
        } else {
            select <- sample(seq(n), size, replace = TRUE)
            freq <- table(table(select))
            fq <- cbind(as.integer(names(freq)), as.integer(freq))
            tmp <- .unique.string()
            res <- as.db.data.frame(sort(x, FALSE, "random"), tmp, FALSE,
                                    FALSE, TRUE, FALSE, NULL, sum(fq[,2]))
            for (i in seq_len(max(fq[,1])-1)+1) {
                sz <- sum(fq[fq[,1]>=i,2])
                sql <- paste("insert into ", content(res), " (",
                             content(sort(x, FALSE, "random")),
                             " limit ", sz, ")", sep = "")
                .get.res(sql = sql, conn.id = conn.id(x))
            }

            .restore.warnings(warnings)

            res@.dim[1] <- size
            res
        }
    })

## -----------------------------------------------------------------------

## Create an intermediate temp table with index
.create.indexed.temp.table <- function (x, random = FALSE)
{
    if (random) x <- sort(x, FALSE, "random")
    tmp <- .unique.string()
    if (is(x, "db.data.frame"))
        y <- as.db.data.frame(x, tmp, FALSE, FALSE, TRUE,
                              NULL, NULL)
    else
        y <- as.db.data.frame(x, tmp, FALSE, FALSE, TRUE, FALSE,
                              NULL, NULL)
    tmp <- db.objects(tmp, conn.id(x))
    id.col <- .unique.string()
    sql <- paste("alter table ", tmp, " add column ", id.col,
                 " bigserial", sep = "")
    re <- .db.getQuery(sql, conn.id = conn.id(x))

    y@.col.name <- c(y@.col.name, id.col)
    y@.col.data_type <- c(y@.col.data_type, "integer")
    y@.col.udt_name <- c(y@.col.udt_name, "int4")
    y@.factor.suffix <- c(y@.factor.suffix, "")
    y@.appear.name <- c(y@.appear.name, id.col)
    y@.is.factor <- c(y@.is.factor, FALSE)
    y@.dim[2] <- y@.dim[2] + 1
    y@.key <- id.col
    y
}
