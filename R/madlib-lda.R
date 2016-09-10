
setClass("lda.madlib")

madlib.lda <- function (data, docid, words, topic_num,
                        alpha, beta, iter_num = 20,
                       ...) {
    if ( ! is( data, "db.obj" ) )
        stop( "madlib.lda can only be used on a db.obj object, ",
             "and ", deparse( substitute( documents ) ), " is not!")

    conn.id <- conn.id(data)
    warnings <- .suppress.warnings(conn.id)
    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id) # MADlib schema name

    tbl.model <- .unique.string()
    tbl.output <- .unique.string()
    tbl.tf <- .unique.string()
    tbl.source <- content(data)

    ##query to get the data in appropriate format for lda_train
    sql_tf <- paste("select ", madlib, ".term_frequency('",
                tbl.source, "','", docid, "','", words, "','", tbl.tf, "',", TRUE,")",sep="")

    db.q(sql_tf, nrows = 1,
                conn.id = conn.id, verbose = FALSE)

    #compute vocabulary size
    sql_voc_size <- paste("select count(*) from ", tbl.tf, "_vocabulary", sep="")
    voc_size <- as.numeric(db.q(sql_voc_size, ";", conn.id = conn.id, verbose= FALSE))

    sql_voc <- paste("select word from ", tbl.tf, "_vocabulary", sep="")
    vocabulary <- db.q(sql_voc, nrows = -1,conn.id = conn.id, verbose = FALSE)
    vocabulary <- t(vocabulary)

    sql <- paste("select ", madlib, ".lda_train('",
                 tbl.tf, "', '", tbl.model, "', '",
                 tbl.output, "',", voc_size, ", ",
                 topic_num, ",", iter_num, ",", alpha,
                 ",", beta, ")", sep = "")

    res_out <- db.q(sql, "; select topic_count, topic_assignment from ", tbl.output, nrows = -1,
                 conn.id = conn.id, verbose = FALSE)

    sql_parse_model <- paste("select (", madlib,
        ".lda_parse_model(model, voc_size, topic_num)).* from ", tbl.model, sep="")

    res_model_tuple <- db.q(sql_parse_model, nrows = -1,
                 conn.id = conn.id, verbose = FALSE)

    model_table <- db.data.frame(tbl.model, conn.id = conn.id, verbose = FALSE)
    output_table <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    tf_table <- db.data.frame(tbl.tf, conn.id = conn.id, verbose = FALSE)

    half_voc <- as.numeric(floor(voc_size/2))
    model1_flatten <- arraydb.to.arrayr(res_model_tuple[[1]], "integer")#, half_voc*topic_num)
    model1 <- matrix(model1_flatten, nrow=topic_num, ncol=half_voc)
    model2_flatten <- arraydb.to.arrayr(res_model_tuple[[2]], "integer")
    model2 <- matrix(model2_flatten, nrow=topic_num, ncol=voc_size-half_voc)
    topic_sums <- arraydb.to.arrayr(res_model_tuple[[3]], "integer", topic_num)
    topic_sums <- t(topic_sums) # to match lda package
    topics<- cbind(model1,model2)
    colnames(topics) <- vocabulary

    document_sums <- arraydb.to.arrayr(res_out$topic_count)
    document_sums <- t(document_sums)

    assignments <- res_out$topic_assignment
    assignments <- lapply(assignments, arraydb.to.arrayr)

    rst <- list()
    rst$assignments <- assignments
    rst$topics <- topics
    rst$topic_sums <- topic_sums
    rst$document_sums <- document_sums
    rst$model_table <- model_table
    rst$output_table <- output_table
    rst$tf_table <- tf_table

    class(rst) <- "lda.madlib"
    .restore.warnings(warnings)
    return (rst)
}

#If the user wants to compute perplexity from prediction output, he can enter it
#as a second parameter; otherwise the output table from madlib.lda_train will
#be used.
perplexity.lda.madlib <- function(object, predict_output_table = NULL,...){
    model_table <- object$model_table
    if (is.null(predict_output_table)){ output_table <- object$output_table}
    else {output_table <- predict_output_table}

    conn.id <- conn.id(model_table)
    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id)

    tbl.model <- content(model_table)
    tbl.output <- content(output_table)

    sql <- paste("select ", madlib,
                 ".lda_get_perplexity('", tbl.model,
                 "','", tbl.output, "')", sep="")

    perplexity <- db.q(sql, conn.id = conn.id, verbose = FALSE)
    perplexity <- as.numeric(perplexity)

    return (perplexity)
}

#Object parameter is the output of the training function
predict.lda.madlib <- function(object, data, docid, words, ...){
    conn.id <- conn.id(data)
    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id)

    model_table <- object$model_table
    tbl.source <- content(data)
    tbl.output <- .unique.string()
    tbl.model <- content(model_table)
    tbl.tf <- .unique.string()

    sql_tf <- paste("select ", madlib, ".term_frequency('",
                tbl.source, "','", docid, "','", words, "','", tbl.tf, "',", TRUE,")",sep="")

    db.q(sql_tf, nrows = 1, conn.id = conn.id, verbose = FALSE)
    sql <- paste("select ", madlib,
                 ".lda_predict('", tbl.tf, "', '", tbl.model,
                 "', '", tbl.output, "')", sep = "")
    .db(sql, conn.id = conn.id, verbose = FALSE)
    #if (is.temp) delete(newdata)
    db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
}
