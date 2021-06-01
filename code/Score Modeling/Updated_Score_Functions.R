
# some base functions for scoring and normalization


##############################
## NORMALIZATION FUNCTIONS
##############################

# Normalize vector to a custom range [x,y]

normalize_vec <- function(vec, x=0.01, y=0.99, log = FALSE) {
  if (log == TRUE) { vec <- log(vec) }
  norm_v <- (vec - min(vec))/(max(vec) - min(vec))
  custom_norm_v <- norm_v*(y - x) + x
  custom_norm_v
}


# Normalize all numeric columns in a dataframe to a custom range [x,y]

normalize_df <- function(df, x = 0.01, y = 0.99, log = FALSE) {
  num_cols <- which(sapply(df, is.numeric)) # numeric columns
  normed <- sapply(df[num_cols], normalize_vec, x = x, y = y, log = log)
  df[num_cols] <- (normed)
  df
}


##############################
## SCORING FUNCTION
##############################


# sum score function : SUM [i..n] (1 / (traveltime_i + 2*std_traveltime_i) + ... ))

sum_score_fxn <- function(df, weight = FALSE, log_normalize_score = FALSE) {

  
  # compute score for each row
  if (weight == FALSE) { df$unique_score <- 1/(df$avg_time+2*df$sd_time) } 
  else { df$unique_score <- (1+df$weight)/(df$avg_time+2*df$sd_time) }
  
  ## sum scores and normalize
  df <- df %>% 
    group_by(fromId, type) %>% 
    summarize(score = sum(unique_score)) %>%
    group_by(type) %>%
    mutate(score = normalize_vec(score, x = 0.01, y = 0.99, log = log_normalize_score))
  
  df
}

