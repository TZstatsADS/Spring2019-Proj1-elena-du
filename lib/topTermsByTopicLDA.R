# this function is written following the tutorial: https://www.kaggle.com/rtatman/nlp-in-r-topic-modelling 

top_terms_by_topic_LDA = function(text, plot = T, number_of_topics = 4) {    
  corpus = getCorpus(text)
  DTM = DocumentTermMatrix(corpus) 
  
  unique_indexes = unique(DTM$i) 
  DTM = DTM[unique_indexes,] 
  
  lda = LDA(DTM, k = number_of_topics, method = "Gibbs", control = list(seed = 1234))
  topics = tidy(lda, matrix = "beta")
  
  top_terms = topics  %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  if(plot == T){
    top_terms %>% 
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) + 
      geom_col(show.legend = FALSE) + 
      facet_wrap(~ topic, scales = "free") + 
      labs(x = NULL, y = "Informativeness ($$\beta$$)") + 
      coord_flip()
  }else{ 
    return(top_terms)
  }
}
