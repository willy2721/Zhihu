# source('./script/zhihu_preprocessing.R')
# library(dplyr)
# library(rJava)
# library(tm)
# library(tmcn)
# library(SnowballC)
# library(slam)
# library(XML)
# library(RCurl)
# library(Rwordseg)
# library(Matrix)


# if (!require('tmcn')) {
#   install.packages('tmcn',repos = 'http://R-Forge.R-project.org')  
# }
# library(tmcn)
# if (!require('Rwordseg')) {
#   install.packages("Rwordseg",repos = 'http://R-Forge.R-project.org')
# }
# library(Rwordseg)


space_tokenizer <- function(x){
  unlist(strsplit(as.character(x[[1]]),'[[:space:]]+'))
}

#text_filter <- function(data_frame) {
# Keep only the columns with text
# data_frame <- data.frame(data_frame$question_title, data_frame$question_detail, data_frame$ans)
# colnames(data_frame) <- c("question_title","question_detail", "ans")

# Remove symbols 
#  data_frame$question_title <- clean_text(data_frame$question_title)
#  data_frame$question_detail <- clean_text(data_frame$question_detail)
#  data_frame$ans <- clean_text(data_frame$ans)


#  data_frame$question_title[is.na(data_frame$question_title)] <- ''
#  data_frame$question_detail[is.na(data_frame$question_detail)] <- ''
#  data_frame$ans[is.na(data_frame$ans)] <- ''

# Remove empty rows or NA
#  return(data_frame)
#}

#stop_word_vector <- function(df) {
#  df$question <- paste(df$question_title, df$question_detail)
#View(df)

#  document <- c(unique(df$question),unique(df$ans))

#  stop_word <- get_stop_word(document)
#  return(stop_word)
#}

tf_idf_score <- function(df){
  # df <- text_filter(df)
  #  stop_words <- stop_word_vector(df)
  #df$ans_seg <- sapply(df$ans, function(x) paste(seg_worker[x], collapse = ' '))
  
  # Transform the entire answer column into a corpus
  d_corpus <- VCorpus(VectorSource(as.vector(df$ans_seg)))
  
  # Remove punctuation
  d_corpus <- tm_map(d_corpus, removePunctuation)
  
  # Remove numbers
  d_corpus <- tm_map(d_corpus, removeNumbers)
  #inspect(d_corpus)
  #print(toTrad(stopwordsCN()))
  
  #Remove stopwords
  #  d_corpus <- tm_map(d_corpus, removeWords, toTrad(stopwordsCN()))
  #  d_corpus <- tm_map(d_corpus, removeWords, stop_words)
  
  # Remove whitespace
  d_corpus = tm_map(d_corpus, stripWhitespace)
  
  # Transform back into vector
  d_corpus <- Corpus(VectorSource(d_corpus))
  
  # Use control list with space tokenizer
  control_list=list(wordLengths=c(2,Inf),tokenize=space_tokenizer)
  tdm <- TermDocumentMatrix(Corpus(VectorSource(d_corpus)), control = control_list)
  
  # Tf-idf computation
  tf <- apply(tdm, 2, sum) # term frequency
  idf <- function(word_doc){ log2( (length(word_doc)) / (nnzero(word_doc)+1)) }
  idf <- apply(tdm, 1, idf)
  dic_tfidf <- as.matrix(tdm)
  for(i in 1:nrow(tdm)){
    for(j in 1:ncol(tdm)){
      dic_tfidf[i,j] <- (dic_tfidf[i,j] / tf[j]) * idf[i]
    }
  }
  
  # Dealing with query
  q = paste(df$question_title[1], df$question_detail[1])
  q_seg <- filter_segment(seg_worker[q], stop_words)
  query_frame <- as.data.frame(table(q_seg))
  query_frame <- query_frame %>% na.omit()
  
  # Get short doc matrix
  all_term <- rownames(dic_tfidf)
  loc <- which(is.element(all_term, query_frame$q_seg))
  s_tdm <- dic_tfidf[loc,]
  query_frame <- query_frame[is.element(query_frame$q_seg, rownames(s_tdm)),]
  s_tdm[is.na(s_tdm)]=0
  
  # Result : cos similarity ranking
  cos_tdm <- function(x, y){ x%*%y / sqrt(x%*%x * y%*%y) }
  #print(s_tdm)
  #print(query_frame)
  doc_cos <- apply(s_tdm, 2, cos_tdm, y = query_frame$Freq)
  doc_cos[is.nan(doc_cos)] <- 0
  return(doc_cos)
  
}