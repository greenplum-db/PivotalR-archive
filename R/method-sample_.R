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

        conn.id <- conn.id(x)
        warnings <- .suppress.warnings(conn.id)

        if (!replace) {
            tmp <- .unique.string()

            res <- as.db.data.frame(sort(x, FALSE, "random"), tmp, FALSE,
                                    FALSE, TRUE, FALSE, x@.dist.by, size)
            .restore.warnings(warnings)
            res
        } else {
            select <- sample(seq(n), size, replace = TRUE)
            freq <- table(table(select))
            fq <- cbind(as.integer(names(freq)), as.integer(freq))

            m <- sum(fq[,2])
            tmp <- .unique.string()
            res <- as.db.data.frame(sort(x, FALSE, NULL), tmp, FALSE,
                                    FALSE, TRUE, FALSE, x@.dist.by, m)
            dist.str <- .get.distributed.by.str(conn.id, res@.dist.by)

            for (i in seq_len(max(fq[,1])-1)+1) {
                sz <- sum(fq[fq[,1]>=i,2])
                if (i == 2) {
                    tmp1 <- .unique.string()
                    sql <- paste("create temp table ", tmp1,
                                 " as select * from ",
                                 content(res), " order by random() limit ",
                                 sz, dist.str,
                                 sep = "")
                } else {
                    sql <- paste("insert into ", tmp1, " (select * from ",
                                 content(res), " order by random() limit ",
                                 sz, ")", sep = "")
                }
                db.q(sql, conn.id = conn.id, verbose = FALSE)
            }

            if (max(fq[,1]) > 1) {
                db.q(paste("insert into ", content(res),
                           " (select * from ", tmp1, ")", sep = ""),
                     conn.id = conn.id, verbose = FALSE)
                delete(tmp1, is.temp = TRUE, cascade = TRUE)
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
    ## if (random) x <- sort(x, FALSE, "random")
    ## tmp <- .unique.string()
    ## if (is(x, "db.data.frame"))
    ##     y <- as.db.data.frame(x, tmp, FALSE, FALSE, TRUE,
    ##                           NULL, NULL)
    ## else
    ##     y <- as.db.data.frame(x, tmp, FALSE, FALSE, TRUE, FALSE,
    ##                           NULL, NULL)
    ## tmp <- db.objects(tmp, conn.id(x))
    ## id.col <- .unique.string()
    ## sql <- paste("alter table ", tmp, " add column ", id.col,
    ##              " bigserial", sep = "")
    ## re <- .db.getQuery(sql, conn.id = conn.id(x))

    ## y@.col.name <- c(y@.col.name, id.col)
    ## y@.col.data_type <- c(y@.col.data_type, "integer")
    ## y@.col.udt_name <- c(y@.col.udt_name, "int4")
    ## y@.factor.suffix <- c(y@.factor.suffix, "")
    ## y@.appear.name <- c(y@.appear.name, id.col)
    ## y@.is.factor <- c(y@.is.factor, FALSE)
    ## y@.dim[2] <- y@.dim[2] + 1
    ## y@.key <- id.col
    ## y

    conn.id <- conn.id(x)
    tmp <- .unique.string()
    id.col <- .unique.string()
    dbms <- (.get.dbms.str(conn.id))$db.str
    if (dbms != "PostgreSQL") {
        dist.cols <- x@.dist.by
        if (identical(dist.cols, character(0)) ||
            !all(dist.cols %in% x@.col.name)) {
            dist.str <- paste("distributed by (", id.col, ")", sep = "")
            dist.by <- id.col
        } else {
            dist.str <- paste("distributed by (",
                              paste("\"", dist.cols, "\"", sep = ""),
                              ")", sep = "")
        }
    } else {
        dist.str <- ""
        dist.by <- ""
    }

    if (random) random.str <- " over (order by random())"
    else random.str <- ""

    .db.getQuery(
        .format("create temp table <tmp> as
                    select *, row_number()<random.str> as <id.col>
                    from (<tbl>) s <dist.str>",
                list(tmp = tmp, id.col = id.col, tbl = content(x[,]),
                     dist.str = dist.str)), conn.id = conn.id)

    db.data.frame(tmp, conn.id = conn.id, is.temp = TRUE)
}
