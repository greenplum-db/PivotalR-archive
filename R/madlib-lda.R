

setClass("lda.madlib")

madlib.lda <- function (data, docid, words, topic_num,
                        alpha, eta, iter_num = 20,
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
    
    #print for debugging
    #print(sql_tf)

    #do we actually want all rows here? is it too big?
    db.q(sql_tf, "; select * from ", tbl.tf, nrows = 1,
                conn.id = conn.id, verbose = FALSE)

    


    
    

    #compute vocabulary size
    sql_voc_size <- paste("select count(*) from ", tbl.tf, "_vocabulary", sep="")
    voc_size <- as.numeric(db.q(sql_voc_size, ";", conn.id = conn.id, verbose= FALSE))


    sql_voc <- paste("select word from ", tbl.tf, "_vocabulary", sep="")
    vocabulary <- db.q(sql_voc, conn.id = conn.id, verbose = FALSE)
    vocabulary <- t(vocabulary)

    #vocabulary <- arraydb.to.arrayr(vocabulary, "character", voc_size)

    #print(voc_size)



    sql <- paste("select ", madlib, ".lda_train('",
                 tbl.tf, "', '", tbl.model, "', '",
                 tbl.output, "',", voc_size, ", ",
                 topic_num, ",", iter_num, ",", alpha, 
                 ",", eta, ")", sep = "")

    #print(sql)
    
    res_out <- db.q(sql, "; select * from ", tbl.output, nrows = -1,
                 conn.id = conn.id, verbose = FALSE)
    
    #print(res_out)


    sql_parse_model <- paste("select (", madlib,
        ".lda_parse_model(model, voc_size, topic_num)).* from ", tbl.model, sep="")

    res_model_tuple <- db.q(sql_parse_model, nrows = -1,
                 conn.id = conn.id, verbose = FALSE)


    half_voc <- as.numeric(floor(voc_size/2))
    model1_flatten <- arraydb.to.arrayr(res_model_tuple[[1]], "integer")#, half_voc*topic_num)
    model1 <- matrix(model1_flatten, nrow=topic_num, ncol=half_voc)
    model2_flatten <- arraydb.to.arrayr(res_model_tuple[[2]], "integer")
    model2 <- matrix(model2_flatten, nrow=topic_num, ncol=voc_size-half_voc)
    topic_sums <- arraydb.to.arrayr(res_model_tuple[[3]], "integer", topic_num)
    topic_sums <- t(topic_sums) # to match lda package

    topics<- cbind(model1,model2)
    colnames(topics) <- vocabulary
    
    # print(res_model_tuple)
    # print(topics)
    # print(topic_sums)

    document_sums <- arraydb.to.arrayr(res_out$topic_count)
    document_sums <- t(document_sums)

    assignments <- res_out$topic_assignment
    assignments <- lapply(assignments, arraydb.to.arrayr)

    #####

    rst <- list()

    rst$assignments <- assignments
    rst$topics <- topics
    rst$topic_sums <- topic_sums
    rst$document_sums <- document_sums

    class(rst) <- "lda.madlib"

    .restore.warnings(warnings)

    return (rst)

    

   
    



    #model <- db.data.frame(tbl.model, conn.id = conn.id, verbose = FALSE)
    #output <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)

    #Goal: output an object of class "lda.madlib" which has attributes:
    #assignments: a list length D of topic assignments for each word (in output table)
    #topics: a K x V matrix where a_ij is number of times word j is assigned to topic i
    #topic_sums: length K vector, topic distribution for corpus 
    #document_sums: K x D matrix, breaking down topic distribution into documents
}