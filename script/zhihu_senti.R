library(readr)
library(jiebaR)
library(dplyr)
source('./script/zhihu_preprocessing.R')

positive <- read_csv('./data/positive.csv')$x
negative <- read_csv('./data/negative.csv')$x

senti = function(df){
  df$pos <- sapply(df$ans_seg_vec,function(x) sum(is.element(x,positive)))
  df$neg <- sapply(df$ans_seg_vec,function(x) sum(is.element(x,negative)))
  df <- df %>% mutate(senti_score = pos/(pos+neg))
  df$senti_score[is.na(df$senti_score)] = 0.5
  return(df$senti_score)
}

#zhihu_data <- read_csv("./data/food.csv")
#zhihu_data <- as.data.frame(zhihu_data)
#senti(zhihu_data)