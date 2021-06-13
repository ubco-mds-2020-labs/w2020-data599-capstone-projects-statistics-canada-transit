
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

# Normalize vector to a custom range [x,y]

normalize_vec_nonAbs <- function(vec, x=-0.99, y=0.99, log = FALSE) {
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
## NA SUBSTITUTION FUNCTIONS
##############################

# function for NA grid expansion 
NA_grid_maker_eff <- function(id, df) {
  all_amenities <- as.character(unique(df$type))
  # get missing amenities by indexing the fromId and keeping only unique types
  missing_amenities <- setdiff(all_amenities, unique(df$type[df$fromId == id]))
  
  # create NA rows to append via expand.grid (creates a row for every factor combination)
  NA_rows <- expand.grid('fromId' = id,
                         'type' = missing_amenities,
                         'score' = NA,
                         'pop' = NA,
                         'lat' = NA,
                         'lon' = NA,
                         'prox_score' = NA,
                         'eff' = NA,
                         'eff_ravg' = NA, 
                         stringsAsFactors = TRUE)
  NA_rows
}

# function for actually filling data.tables with NA values
NA_table_filler_eff <- function(df, custom_idx = NULL) {
  # count each fromId occurence
  fromId_counts <- df %>% group_by(fromId) %>% mutate(n = n())

  if (is.null(custom_idx)) {
    # create a fromId array using Ids that don't meet the [x] count requirement
    id_arr <- array(unique(fromId_counts[fromId_counts$n < x, ]$fromId))
  } else {
    id_arr <- custom_idx
  }

  # get rows
  filler_rows <- rbindlist(apply(id_arr, MARGIN = 1, FUN = NA_grid_maker_eff, df = df))
  # append and order
  df <- rbindlist(list(df, filler_rows), use.names = TRUE) 
  
  df <- df %>% arrange(fromId, type)
  
  
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
  
  # variable vector
  variable <- polyg_subset$eff_ravg
  
  # colour palette 
  #  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  Rd2Gn <- c("#800080","#0000FF","#00FFFF", "#00FF00", "#FFFF00", "#FFA500", "#FF0000")
  pal_fun <- colorNumeric(palette = Rd2Gn, NULL, n = 7)
  
  # popup # percentile(score_vec),
  percentile <- ecdf(polyg_subset$score)
  p_popup <- paste0("Accessibility Percentile: <strong>", round(percentile(polyg_subset$score), 2)*100, '%',"</strong>", 
                    "<br>Block Population: <strong>",  round(as.numeric(polyg_subset$pop.x), 2),"</strong>",
                    "<br>Efficiency score: <strong>",  round(polyg_subset$eff, 2),"</strong>",
                    "<br><br>Block ID: ", polyg_subset$DBUID,
                    "<br>Running Efficiency Score: ", round(variable, 2))
  
  
  map <- leaflet(data = polyg_subset) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(variable), # set fill colour with pallette fxn from aboc
      fillOpacity = 0.6, smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
    setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
    addLegend("bottomleft",  # location
              pal=pal_fun,    # palette function
              values=~variable,  # value to be passed to palette function
              title = glue('{amn_name} Efficiency Access'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}

map_maker_efficiency_quant <- function(data, amenity, output_dir, view_map = FALSE) {
  
  amn_name <- amenity %>%
    str_to_title() %>%
    str_replace_all('Or', 'or') %>%
    str_replace('And', 'and') %>%
    str_replace('/Performance', '')
  
  file_name <- glue('{amn_name} efficiency map')
  print(paste('Current Map:', file_name))
  
  # subset info
  polyg_subset <- data[data$type == amenity, ]
  
  # variable vector
  variable <- polyg_subset$eff_ravg
  
  # colour palette 
  #  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  Rd2Gn <- c("#800080","#0000FF","#00FFFF", "#00FF00", "#FFFF00", "#FFA500", "#FF0000")
  pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 7)
  
  # popup # percentile(score_vec),
  percentile <- ecdf(polyg_subset$score)
  p_popup <- paste0("Accessibility Percentile: <strong>", round(percentile(polyg_subset$score), 2)*100, '%',"</strong>", 
                    "<br>Block Population: <strong>",  round(as.numeric(polyg_subset$pop.x), 2),"</strong>",
                    "<br>Efficiency score: <strong>",  round(polyg_subset$eff, 2),"</strong>",
                    "<br><br>Block ID: ", polyg_subset$DBUID,
                    "<br>Running Efficiency Score: ", round(variable, 2))
  
  
  map <- leaflet(data = polyg_subset) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(variable), # set fill colour with pallette fxn from aboc
      fillOpacity = 0.6, smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
    setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
    addLegend("bottomleft",  # location
              pal=pal_fun,    # palette function
              values=~variable,  # value to be passed to palette function
              title = glue('{amn_name} Efficiency Access'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}
