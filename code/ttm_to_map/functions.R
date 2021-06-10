
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
## NA SUBSTITUTION FUNCTIONS
##############################

# function for NA grid expansion 
NA_grid_maker <- function(id, df, isochrone = FALSE, efficiency = FALSE) {
  all_amenities <- as.character(unique(df$type))
  # get missing amenities by indexing the fromId and keeping only unique types
  missing_amenities <- setdiff(all_amenities, unique(df$type[df$fromId == id]))
  
  if (efficiency == TRUE) {
    # create NA rows to append via expand.grid (creates a row for every factor combination)
    NA_rows <- expand.grid('fromId' = id,
                           'type' = missing_amenities,
                           'pop' = NA,
                           'lat' = NA,
                           'lon' = NA,
                           'eff_ravg' = NA, 
                           stringsAsFactors = TRUE)
  } else if (isochrone == FALSE) {
    # create NA rows to append via expand.grid (creates a row for every factor combination)
    NA_rows <- expand.grid('fromId' = id,
                         'type' = missing_amenities,
                         'weight' = as.character(unique(df$weight)),
                         'nearest_n' = as.character(unique(df$nearest_n)),
                         'score' = NA, 
                         stringsAsFactors = TRUE)
  } else {
    # create NA rows to append via expand.grid (creates a row for every factor combination)
    NA_rows <- expand.grid('fromId' = id,
                         'type' = missing_amenities,
                         'time_groups' = NA, 
                         stringsAsFactors = TRUE)
  }
  NA_rows
}

# function for actually filling data.tables with NA values
NA_table_filler <- function(df, custom_idx = NULL, isochrone = FALSE, efficiency = FALSE) {
  # count each fromId occurence
  fromId_counts <- df %>% group_by(fromId) %>% mutate(n = n())

  if (is.null(custom_idx)) {
    # create a fromId array using Ids that don't meet the [x] count requirement
    id_arr <- array(unique(fromId_counts[fromId_counts$n < x, ]$fromId))
  } else {
    id_arr <- custom_idx
  }

  # get rows
  filler_rows <- rbindlist(apply(id_arr, MARGIN = 1, FUN = NA_grid_maker, df = df, isochrone = isochrone, efficiency = efficiency))
  # append and order
  df <- rbindlist(list(df, filler_rows), use.names = TRUE) 
  
  if (isochrone == FALSE & efficiency == FALSE) {
    df <- df %>% arrange(fromId, type, nearest_n, weight)
  } else {
    df <- df %>% arrange(fromId, type)
  }
  
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

# Mapping function for score tables
map_maker_scores <- function(data, amenity, weight, nearest_n, output_dir, view_map = FALSE) {
  
  amn_name <- amenity %>%
                str_to_title() %>%
                str_replace_all('Or', 'or') %>%
                str_replace('And', 'and') %>%
                str_replace('/Performance', '')

  file_name <- glue('{amn_name} - wt({weight}) - n({str_to_upper(nearest_n)})')
  print(paste('Current Map:', file_name))

  # subset info
  polyg_subset <- data[data$type == amenity & data$weight == weight & data$nearest_n == nearest_n, ]
  
  # score vector
  score_vec <- polyg_subset$score
  
  # colour palette 
  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 5)
  
  # popup # percentile(score_vec),
  percentile <- ecdf(score_vec)
  p_popup <- paste0("Accessibility Percentile: <strong>", round(percentile(score_vec), 2)*100, '%',"</strong>", 
                    "<br>Block Population: <strong>", polyg_subset$pop,"</strong>",
                    "<br><br>Block ID: ", polyg_subset$DBUID,
                    "<br>Raw Score: ", round(score_vec, 2))
  
  map <- leaflet(data = polyg_subset) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(score_vec), # set fill colour with pallette fxn from aboc
      fillOpacity = 0.6, smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
    setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
    addLegend("bottomleft",  # location
              pal=pal_fun,    # palette function
              values=~score_vec,  # value to be passed to palette function
              title = glue('{amn_name} Transit Access'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}

# Efficiency maps
map_maker_efficiency <- function(data, amenity, output_dir, view_map = FALSE) {
    
    amn_name <- amenity %>%
      str_to_title() %>%
      str_replace_all('Or', 'or') %>%
      str_replace('And', 'and') %>%
      str_replace('/Performance', '')
    
    file_name <- glue('{amn_name} efficiency map')
    print(paste('Current Map:', file_name))
    
    # subset info
    polyg_subset <- data[data$type == amenity, ]
    
    # score vector
    score_vec <- polyg_subset$eff_ravg
    
    # colour palette 
    Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
    pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 5)
    
    # popup # percentile(score_vec),
    percentile <- ecdf(score_vec)
    p_popup <- paste0("Accessibility Percentile: <strong>", round(percentile(score_vec), 2)*100, '%',"</strong>", 
                      "<br>Block Population: <strong>", polyg_subset$pop,"</strong>",
                      "<br><br>Block ID: ", polyg_subset$DBUID,
                      "<br>Running Efficiency Score: ", round(score_vec, 2))
  
        
  map <- leaflet(data = polyg_subset) %>%
      addPolygons(
        stroke = FALSE,  # remove polygon borders
        fillColor = ~pal_fun(score_vec), # set fill colour with pallette fxn from aboc
        fillOpacity = 0.6, smoothFactor = 0.5, # aesthetics
        popup = p_popup) %>% # add message popup to each block
      addTiles() %>%
      setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
      addLegend("bottomleft",  # location
                pal=pal_fun,    # palette function
                values=~score_vec,  # value to be passed to palette function
                title = glue('{amn_name} Transit Access'))
    
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}


# Mapping function for isochrone tables
map_maker_isochrone <- function(data, amenity, output_dir, view_map = FALSE) {
  
  amn_name <- amenity %>%
                str_to_title() %>%
                str_replace_all('Or', 'or') %>%
                str_replace('And', 'and') %>%
                str_replace('/Performance', '')
  
  file_name <- glue('{amn_name} Transit Isochrone')
  print(paste('Current Map:', file_name))

  # subset info
  polyg_subset <- data[data$type == amenity, ]
  
  # score vector
  time_groups <- polyg_subset$time_groups
  
  # colour palette 
  pal_fun <- colorFactor(
    palette = c("#3ef000", "#c5eb00", "#fbff00", "#e9cb00", "#e78600", "#e44200", "#e20000"),
    levels = sort(unique(polyg_subset$time_groups))
    )

  p_popup <- paste0("Nearest: <strong>", amn_name,"</strong>", 
                    "<br>Travel Time: <strong><", time_groups, " minutes</strong>")
        
  map <- leaflet(data = polyg_subset) %>%
      addPolygons(
        stroke = FALSE,  # remove polygon borders
        fillColor = ~pal_fun(time_groups), # set fill colour with pallette fxn from aboc
        fillOpacity = 0.7, smoothFactor = 0.5, # aesthetics
        popup = p_popup) %>% # add message popup to each block
      addTiles() %>%
      setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
      addLegend("bottomleft",  # location
                pal=pal_fun,    # palette function
                values=~time_groups,  # value to be passed to palette function
                title = glue('{amn_name} Transit Access'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}