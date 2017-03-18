library(jiebaR)
library(dplyr)

seg_worker = worker()

# 清理文本
clean_text <-  function(text){
  
  # 濾網頁標籤元素
  text <- gsub('<br>','',text)
  
  text <- gsub('<ul.*?>','',text)
  text <- gsub('</ul>','',text)
  text <- gsub('<li.*?>','',text)
  text <- gsub('</li>','',text)
  text <- gsub('<ol.*?>','',text)
  text <- gsub('</ol>','',text)
  
  text <- gsub('<blockquote>','',text)
  text <- gsub('</blockquote>','',text)
  text <- gsub('<strong>','',text)
  text <- gsub('</strong>','',text)
  text <- gsub('<i.*?>','',text)
  text <- gsub('</i>','',text)
  text <- gsub('<u.*?>','',text)
  text <- gsub('</u>','',text)
  text <- gsub('<b.*?>','',text)
  text <- gsub('</b>','',text)
  text <- gsub('<p.*?>','',text)
  text <- gsub('</p>','',text)
  
  text <- gsub('&gt;','',text)
  text <- gsub('&lt;','',text)
  text <- gsub('&amp;','',text)
  text <- gsub('&quot;','',text)
  text <- gsub('&nbsp;','',text)
  
  text <- gsub('<img.*?>','',text)
  
  text <- gsub('<a.*?>','',text)
  text <- gsub('</a>','',text)
  
  text <- gsub('<span.*?>','',text)
  text <- gsub('</span>','',text)
  
  # 濾 url
  text<- gsub('https?://(\\w|[[:punct:]])*','',text)
  
  # 濾過多的重複無意義字元
  text <- gsub('\n','',text)
  text <- gsub('\t','',text)
  text <- gsub('\r','',text)
  text <- gsub('\\s\\s+','',text)
  text <- gsub('[[:punct:]][[:punct:]][[:punct:]]+','',text)
  
  return(text)
}


# 排除未被按讚答案
number_filter <- function(df) {
  return(subset(df, df$ans_upvote_num != 0))
}


# clean_text and omit na
text_filter <- function(df) {
  
  # Remove symbols 
  df$question_title <- clean_text(df$question_title)
  df$question_detail <- clean_text(df$question_detail)
  df$ans <- clean_text(df$ans)
  
  
  df$question_title[is.na(df$question_title)] <- ''
  df$question_detail[is.na(df$question_detail)] <- ''
  df$ans[is.na(df$ans)] <- ''
  # Remove empty rows or NA
  df[df==""] <- NA
  return(df %>% na.omit())
}


# 取得 stop word
# library jiebaR at first
get_stop_word <- function(document){
  
  # seg_worker <- worker()
  
  # prevent open error
  document <- gsub('/var/log', 'var log', document)
  
  # method1
  doc_seg_list <- list(c(seg_worker['The Seg List']))
  for (doc in document){
    if(is.na(doc)){
      doc_seg_list <- c(doc_seg_list,list(NA))
    }else if(doc == '.'){
      doc_seg_list <- c(doc_seg_list,list(NA))
    }else{
      doc_seg_list <- c(doc_seg_list,list(c(seg_worker[doc])))
    }
  }
  doc_seg_list <- doc_seg_list[2:length(doc_seg_list)]
  doc_idf <- get_idf(doc_seg_list, stop_word = NULL)
  doc_idf_sorted <- doc_idf[order(-doc_idf$count),]
  stop_word <- doc_idf_sorted$name[1:as.integer((0.0025*nrow(doc_idf_sorted)))]
  stop_word <- as.vector(stop_word)
  return (stop_word)
  
}

get_stop_word_df <- function(document){
  
  # seg_worker <- worker()
  
  # prevent open error
  document <- gsub('/var/log', 'var log', document)
  
  # method1
  doc_seg_list <- list(c(seg_worker['The Seg List']))
  for (doc in document){
    if(is.na(doc)){
      doc_seg_list <- c(doc_seg_list,list(NA))
    }else if(doc == '.'){
      doc_seg_list <- c(doc_seg_list,list(NA))
    }else{
      doc_seg_list <- c(doc_seg_list,list(c(seg_worker[doc])))
    }
  }
  doc_seg_list <- doc_seg_list[2:length(doc_seg_list)]
  doc_idf <- get_idf(doc_seg_list, stop_word = NULL)
  doc_idf_sorted <- doc_idf[order(-doc_idf$count),]
  stop_word_df <- doc_idf_sorted[1:as.integer((0.0025*nrow(doc_idf_sorted))),]
  stop_word_df$freqRatio <- stop_word_df$count/length(document)
  colnames(stop_word_df) <- c('word','count','freqRatio')
  
  return (stop_word_df)
}



