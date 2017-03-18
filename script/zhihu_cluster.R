# library(readr)
# library(dplyr)
# library(jiebaR)
# library(tidytext)
# library(fpc)
# library(cluster)
# library(rJava)
# library(tm)
# library(SnowballC)
# library(slam)
# library(XML)
# library(RCurl)
# library(Matrix)
# library(tmcn)
# library(Rwordseg)
# 
# source('./script/zhihu_preprocessing.R')


# ans 是一在同一問題下的所有資料
# ans 輸入進去後會回傳增加  km1, km2, km3, pc1, p2, pc3 的 dataframe


get_cluster_feature <- function(ans){
  
  kmIterMax = 10000
  
  
  ans$aid <- seq(1:nrow(ans))
  
  ans_word <- ans
  data_number <- nrow(ans_word)
  # print(data_number)
  
  ####################################################################################
  
  space_tokenizer <- function(x){
    unlist(strsplit(as.character(x[[1]]),'[[:space:]]+'))
  }
  
  # Transform the entire answer column into a corpus
  d_corpus <- VCorpus(VectorSource(as.vector(ans_word$ans_seg)))
  
  # Remove punctuation
  d_corpus <- tm_map(d_corpus, removePunctuation)
  
  # Remove numbers
  d_corpus <- tm_map(d_corpus, removeNumbers)
  #inspect(d_corpus)
  #print(toTrad(stopwordsCN()))
  
  #Remove stopwords
  d_corpus <- tm_map(d_corpus, removeWords, toTrad(stopwordsCN()))
  d_corpus <- tm_map(d_corpus, removeWords, stop_word)
  
  # Remove whitespace
  d_corpus = tm_map(d_corpus, stripWhitespace)
  
  # Transform back into vector
  d_corpus <- Corpus(VectorSource(d_corpus))
  
  # Use control list with space tokenizer
  control_list=list(wordLengths=c(2,Inf),tokenize=space_tokenizer)
  tdm <- TermDocumentMatrix(Corpus(VectorSource(d_corpus)), control = control_list)
  
  # # Tf-idf computation
  # tf <- apply(tdm, 2, sum) # term frequency
  # idf <- function(word_doc){ log2( (length(word_doc)) / (nnzero(word_doc)+1)) }
  # idf <- apply(tdm, 1, idf)
  # ans_km_input <- as.matrix(tdm)
  # for(i in 1:nrow(tdm)){
  #   for(j in 1:ncol(tdm)){
  #     ans_km_input[i,j] <- (ans_km_input[i,j] / tf[j]) * idf[i]
  #   }
  # }
  
  ans_km_input <- as.matrix(tdm)
  ans_km_input <- as.data.frame(ans_km_input)
  ans_km_input[is.na(ans_km_input)] <- 0
  
  # ans_km_input <- ans_km_input[rowSums(ans_km_input)> 0.2,]
  ans_km_input <- ans_km_input[rowSums(ans_km_input)> 1,]
  ans_km_input <- ans_km_input[rowSums(ans_km_input)< 50,]
  
  #ans_km_input <- and_tfidf*1000
  
  
  ans_km_input <- t(ans_km_input)
  
  
  t_ans_kmeans <- kmeans(t(ans_km_input), 3, iter.max = kmIterMax, nstart = 100)
  t_ans_kmeans$cluster <- as.factor(t_ans_kmeans$cluster)
  
  # plotcluster(t(ans_km_input), t_ans_kmeans$cluster)
  # clusplot(t(ans_km_input), t_ans_kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
  
  
  ans_kmeans <- kmeans(ans_km_input, 3, iter.max = kmIterMax, nstart = 100)
  ans_kmeans$cluster <- as.factor(ans_kmeans$cluster)
  
  # plotcluster(ans_km_input, ans_kmeans$cluster)
  # clusplot(ans_km_input, ans_kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
  
  
  ans_pca <- prcomp(ans_km_input,
                    center = TRUE, # standarization
                    scale. = TRUE) 
  
  ans_rotated <- ans_pca$x %>% # data projected in pca space
    as.data.frame()
  
  
  ans_PCACluster <- kmeans(ans_rotated[, 1:2], 3, iter.max = kmIterMax, nstart = 100) # run kmeans model
  ans_PCACluster$cluster <- as.factor(ans_PCACluster$cluster)
  
  cluster_feature <- data.frame(
    
    km1 = t_ans_kmeans$centers[1,],
    km2 = t_ans_kmeans$centers[2,],
    km3 = t_ans_kmeans$centers[3,],
    
    
    pc1 = ans_rotated$PC1,
    pc2 = ans_rotated$PC2,
    pc3 = ans_rotated$PC3
  )
  
  return(cluster_feature)
  
}





# 使用範例


#zhihu_data <- read_csv("./data/photography.csv")
#zhihu_data <- as.data.frame(zhihu_data)

#ans_count_limit = 200
#qid = 1

# 篩選回答數夠多的問題
#zhihu_data <- zhihu_data %>% 
#  group_by(question_title) %>%
#  mutate(ans_count = n()) %>%
#  ungroup() %>%
#  filter(ans_count > ans_count_limit)


# 透過 qid 找出鎖定某一問題
#ans <- zhihu_data[zhihu_data$question_title == unique(zhihu_data$question_title)[qid],]


#ans <- get_cluster_feature(ans)
#View(ans)

















