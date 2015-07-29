

setClass("lda.madlib")

madlib.lda <- function (documents, topic_num, voc_size, vocab, 
                        alpha, beta, na.action = NULL,
                       na.as.level = FALSE,
                       ...) # param name too long

{

    if ( ! is( documents, "db.obj" ) )
        stop( "madlib.lda can only be used on a db.obj object, ",
             "and ", deparse( substitute( documents ) ), " is not!")
    if ( ! is( vocab, "db.obj" ) )
        stop( "madlib.lda can only be used on a db.obj object, ",
             "and ", deparse( substitute( vocab) ), " is not!")

    origin.documents <- documents

    conn.id <- conn.id(documents)

    db <- .get.dbms.str(conn.id)


    ## Do we need something similar here?
    #.check.madlib.version(data)
    #if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
     #   stop("MADlib on HAWQ 1.1 does not support the latest random ",
      #       "forest module!")

    warnings <- .suppress.warnings(conn.id)


    madlib <- schema.madlib(conn.id) # MADlib schema name
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str)) {
        tbl.output <- NULL
        sql <- paste("select (f).* from (select ", madlib, ".linregr(",
                     params$dep.str, ",", params$ind.str, ") as f from ",
                     tbl.source, ") s", sep = "")
    } else {
        tbl.output <- .unique.string()
        sql <- paste("select ", madlib, ".linregr_train('",
                     tbl.source, "', '", tbl.output, "', '",
                     params$dep.str, "', '", params$ind.str, "', ",
                     grp, ", ", hetero, ")", sep = "")
    }








}