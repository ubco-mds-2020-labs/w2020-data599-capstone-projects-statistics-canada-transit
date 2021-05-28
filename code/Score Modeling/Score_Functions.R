
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
## SCORING FUNCTIONS
##############################

# naive score function : accessible_points / (mean * std)

naive_score <- function(fromIds, mean_time, mean_sd_time, n_accessible, x=0.01, y=0.99, log = FALSE) {
  
  # normalize vectors prior to score computation
  mean_time <- normalize_vec(mean_time)
  mean_sd_time <- normalize_vec(mean_sd_time)
  n_accessible <- normalize_vec(n_accessible)
  
  # score
  score <- n_accessible / (mean_time*mean_sd_time)
  
  # normalize score with custom parameters
  norm_score <- normalize_vec(score, x = x, y = y, log = log)
  
  df <- data.frame('fromId' = as.factor(fromIds), 'score' =  norm_score)
  #df <- df[order(df$norm_score, decreasing=TRUE, na.last=FALSE), ] # order doesn't matter
  df
}


# sum score function : SUM [i..n] (1 / (traveltime_i * std_traveltime_i) + ... ))

sum_score_fxn <- function(df, weight = FALSE, log_normalize_score = TRUE, normalize_df = FALSE, x=1, y=10) {
  
  
  # custom range data normalization prior to score computation
  # log is false since we don't care about correcting skew at this point
  if (normalize_df == TRUE) { df <- normalize_df(df, x = x, y = y, log = FALSE) }
  
  # compute score for each row
  if (weight == FALSE) { df$unique_score <- 1/(df$avg_time*df$sd_time) } 
  else { df$unique_score <- df$weight/(df$avg_time*df$sd_time) }
  
  ## sum scores and normalize (OPTION 1)
  df <- df %>% 
      group_by(fromId, type) %>% 
      summarise(score = sum(unique_score)) %>%
      group_by(type) %>%
      mutate(score = normalize_vec(score, x = 0.01, y = 0.99, log = log_normalize_score))
  
  ## sum scores and normalize (OPTION 2) - less efficient
  #df <- df %>% 
  #    group_by(fromId, type) %>%
  #    summarise(score = sum(unique_score))
  
  # split df by amenity type
  #df_list <- split(df, df$type)
  
  # normalize each df
  #df_list <- lapply(df_list, normalize_df, x = 0.01, y = 0.99, log = log_normalize_score)
  
  # recombine
  #df <- data.table::rbindlist(df_list) %>% arrange(fromId)
  
  df
}

# simplest score function : SUM [i..n] (1 / mean_time

simple_sum_score_fxn <- function(df, weight = FALSE, log_normalize_score = TRUE, normalize_df = FALSE, x=0.01, y=0.99) {
  
  if (normalize_df == TRUE) {
    # 0.01 - 0.99 vector normalization prior to score computation
    # log is false since we don't care about correcting skew at this point
    df <- normalize_df(df, x = x, y = y, log = FALSE)
  }

  if (weight == FALSE) {
    df <- df %>% 
      group_by(fromId, toId) %>%
      summarise(unique_score = 1/(avg_time)) %>% 
      group_by(fromId) %>%
      summarise(score = sum(unique_score))
    
  } else {
    df <- df %>% 
      group_by(fromId, toId) %>%
      summarise(unique_score = weight/(avg_time)) %>% 
      group_by(fromId) %>%
      summarise(score = sum(unique_score))
  }
  # normalize the score
  # by default it will log normalize the score
  df$score <- normalize_vec(df$score, x = 0.01, y = 0.99, log = log_normalize_score)
  
  df
}


# sum score function for pre filtered nearest n dataframes: SUM [i..n] (1 / (traveltime_i * std_traveltime_i) + ... ))

nearest_sum_score_fxn <- function(df, weight = FALSE, log_normalize_score = TRUE, normalize_df = FALSE, x=1, y=10) {
  
  if (normalize_df == TRUE) {
    # 0.01 - 0.99 vector normalization prior to score computation
    # log is false since we don't care about correcting skew at this point
    df <- normalize_df(df, x = x, y = y, log = FALSE)
  }
  
  if (weight == FALSE) {
    df <- df %>% 
      mutate(unique_score = 1/(avg_time*sd_time)) %>% 
      group_by(fromId) %>%
      summarise(score = sum(unique_score))
    
  } else {
    df <- df %>% 
      mutate(unique_score = weight/(avg_time*sd_time))
      group_by(fromId) %>%
      summarise(score = sum(unique_score))
  }
  
  # normalize the score
  # by default it will log normalize the score
  df$score <- normalize_vec(df$score, x = 0.01, y = 0.99, log = log_normalize_score)
  
  df
}