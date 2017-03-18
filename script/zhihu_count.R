library(dplyr)
library(tm)
library(tmcn)

char_count = function(df){
  return(nchar(df$ans))
}

word_count = function(df){
  return(sapply(gregexpr("\\W+", df$ans_seg), length) + 1)
}


stop_word_count <- function(df,sw){
  stop_num <- rep(0, length(df$ans))
  for(item in sw)
  {
    stop_num <- stop_num + sapply(gregexpr(item, df$ans, fixed = TRUE), function(x) sum(x > -1))
  }
  return(stop_num)
}

stop_word_percentage <- function(df) {
  return (df$n_stop / df$n_word)
}

# USAGE
#photography <- tbl_df(read_csv("~/dsR/data/final_data/photography.csv"))
#photography$ans <- clean_text(photography$ans)
#photography$ans[is.na(photography$ans)] <- ''
#photography$char_count <- char_count(photography)
#photography$word_count <- word_count(photography)
#photography$stop_word_count <- stop_word_count(photography)
#photography$stop_word_percentage <- stop_word_percentage(photography)
#photography$stop_word_percentage
#photography$word_count

