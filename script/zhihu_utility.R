
# Calculate response time
time_transform <- function(df) {
  return(ceiling((df$ans_time - df$question_time)/60/60/24))
}

# Categorize answers into good or bad (TO BE DECIDED)
get_ans_quality <- function(df) {
  
  # Calculate log of number of upvotes
  # df$ans_upvote_log <- log(df$ans_upvote_num)
  
  # Use the mean of log as threshold
  # good_thresh <- mean(df$ans_upvote_num)
  
  # Use the median as threshold 
  good_thresh <- median(df$ans_upvote_num)
  df$quality <- ifelse(df$ans_upvote_num > good_thresh, "good answer","bad answer")
  return(df$quality)
}

# Sample taking with adequate good/bad answer proportions
take_sample <- function(df) {
  # Select 30% for each group
  df %>% group_by(quality) %>% sample_frac(0.3)
}