
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

# sum score function : SUM [i..n] (1 / (traveltime_i + 2*std_traveltime_i) + ... ))

sum_score_fxn <- function(df, nearest_n = NULL, weight = FALSE, log_normalize_score = FALSE, x = 0, y = 1) {
  
  # if no nearest n was specified, then get score for all travel time matrix trips
  # if a nearest n was specified then index the nearest_n trip times
  # if no weight was specified then don't use amenity weights in the score
  if (!is.null(nearest_n) & weight == FALSE) {
    # keep only nearest_n travel times
    df <- df %>%
      group_by(fromId, type) %>%
      summarize(avg_time = na.omit(sort(avg_time)[1:nearest_n]),
                sd_time = sd_time[which(avg_time == na.omit(sort(avg_time)[1:nearest_n]))])
    # compute score for each row 
    df$unique_score <- 1 / (df$avg_time + 2*df$sd_time)

  } else if (!is.null(nearest_n) & weight == TRUE) {
    # keep only nearest_n travel times and include weights
    df <- df %>%
      group_by(fromId, type) %>%
      summarize(avg_time = na.omit(sort(avg_time)[1:nearest_n]),
                sd_time = sd_time[which(avg_time == na.omit(sort(avg_time)[1:nearest_n]))],
                weight = weight[which(avg_time == na.omit(sort(avg_time)[1:nearest_n]))])
    # compute score for each row and include weights
    df$unique_score <- (1 + df$weight) / (df$avg_time + 2*df$sd_time) 

  } else if (is.null(nearest_n) & weight == FALSE) {
    # include all rows and compute scores for each row with weight
    df$unique_score <- 1 / (df$avg_time + 2*df$sd_time) 

  } else if (is.null(nearest_n) & weight == TRUE) {
    # include all rows and compute scores for each row 
    df$unique_score <- (1 + df$weight) / (df$avg_time + 2*df$sd_time) 
  }

  ## sum scores and normalize
  df <- df %>% 
    group_by(fromId, type) %>% 
    summarize(score = sum(unique_score)) %>%
    group_by(type) %>%
    mutate(score = normalize_vec(score, x = x, y = y, log = log_normalize_score),
           weight = as.factor(ifelse(weight == FALSE, 'no', 'yes')),
           nearest_n = as.factor(ifelse(is.null(nearest_n), 'all', as.character(nearest_n))))
  
  df
}

##############################
## VISUALIZATION FUNCTIONS
##############################

# function to plot 2 score set distributions by type for exploratory comparison

plot_densities <- function(score_frame1, score_frame2, titl1 = 'Plot 1', titl2 = 'Plot 2') {
  x <- score_frame1 %>%
        ggplot(aes(x = score, color = type)) +
        geom_density() +
        egg::theme_article() +
        theme(aspect.ratio = 0.3) +
        ggtitle(titl1)
  y <- score_frame2 %>%
        ggplot(aes(x = score, color = type)) +
        geom_density() +
        egg::theme_article() +
        theme(aspect.ratio = 0.3)+
        ggtitle(titl2)
  gridExtra::grid.arrange(x, y)
}