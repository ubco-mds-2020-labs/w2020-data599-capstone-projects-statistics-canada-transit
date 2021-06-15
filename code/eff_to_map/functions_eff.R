
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
## NA SUBSTITUTION FUNCTIONS
##############################

# function for NA grid expansion 
NA_grid_maker_eff <- function(id, df) {
  
  # create NA rows to append via expand.grid (creates a row for every factor combination)
  NA_rows <- expand.grid('fromId' = id,
                         'mean_score' = NA,
                         'pop' = NA,
                         'amn_dens' = NA,
                         'trafficScore' = NA,
                         'need' = NA,
                         'eff' = NA,
                         stringsAsFactors = TRUE)
  NA_rows
}

# function for actually filling data.tables with NA values
NA_table_filler_eff <- function(df, custom_idx = NULL,  x = 1) {
  # count each fromId occurence
  fromId_counts <- df %>% group_by(fromId) %>% mutate(n = n())

  if (is.null(custom_idx)) {
    # create a fromId array using Ids that don't meet the [x] count requirement
    id_arr <- array(unique(fromId_counts[fromId_counts$n <= x, ]$fromId))
  } else {
    id_arr <- custom_idx
  }

  # get rows
  filler_rows <- rbindlist(apply(id_arr, MARGIN = 1, FUN = NA_grid_maker_eff, df = df))
  # append and order
  df <- rbindlist(list(df, filler_rows), use.names = TRUE) 
  
  df <- df %>% arrange(fromId)
  
  
  df
}

##############################
## RUNNING AVERAGE FUNCTIONS
##############################


# Get mean traffic count within 500m of each block
db_trafic <- function(row){
  mean(filter(trafic_data, trafic_data$LATITUDE <= (as.numeric(row["lat"])+0.0675) & 
                trafic_data$LATITUDE >= (as.numeric(row["lat"])-0.0675) & 
                trafic_data$LONGITUDE <= (as.numeric(row["lon"])+0.0675) & 
                trafic_data$LONGITUDE >= (as.numeric(row["lon"])-0.0675))$TraficCount) 
}


# Iterate Through each row
# Get the most recent traffic survey for each row
is_NA <- function(row, col = 21){
  if(is.na(row[col+3]) & col>0){
    is_NA(row = row, col = col-1)
  } else if(col == 0 ){
    0
  } else {
    row[col+3]  
  }
}
getAll_Data <- function(row){
  is_NA(row)
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
map_maker_efficiency_num <- function(data, mapType = "Efficiency NumericV3", output_dir, view_map = FALSE) {
  
  amn_name <- mapType
  
  file_name <- glue('{amn_name} map')
  print(paste('Current Map:', file_name))
  
  # subset info
  polyg_subset <- data
  
  # variable vector
  variable <- polyg_subset$eff
  
  # colour palette 
  Bl2Rd <- c("#FF0000", "#FA8072", "#F9A7B0", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#ADDFFF", "#1589FF", "#0000FF")
  pal_fun <- colorNumeric(palette = Bl2Rd, NULL, n = 9)
  
  # popup # percentile(score_vec),
  percentile <- ecdf(polyg_subset$mean_score)
  p_popup <- paste0("Efficiency Score: ", round(variable, 2),
                    "<br><br>Accessibility Score: <strong>", round(polyg_subset$mean_score, 2),"</strong>",
                    "<br>Needs Score:  <strong>", round((1/3)*(polyg_subset$pop+ polyg_subset$trafficScore+ polyg_subset$amn_dens), 2),
                    "<br><br>Population Score: <strong>",  round(polyg_subset$pop, 2),"</strong>",
                    "<br>Traffic score: <strong>",  round(polyg_subset$trafficScore, 2),"</strong>",
                    "<br>Facility density: <strong>",  round(polyg_subset$amn_dens, 2),"</strong>",
                    "<br><br>Block ID: ", polyg_subset$DBUID)
  
  
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
              title = glue('{amn_name} Map'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}

map_maker_efficiency_quant <- function(data, mapType = "Efficiency QuantileV3", output_dir, view_map = FALSE) {
  
  amn_name <- mapType
  
  file_name <- glue('{amn_name} map')
  print(paste('Current Map:', file_name))
  
  # subset info
  polyg_subset <- data
  
  # variable vector
  variable <- polyg_subset$eff
  
  # colour palette 
 # A2RV1 <- c("#8B0000", "#FA8072", "#F9A7B0", "#FFFFFF", "#ADDFFF", "#1589FF", "#0000A5")
  Bl2Rd <- c("#FF0000", "#FA8072", "#F9A7B0", "#FFFFFF", "#ADDFFF", "#1589FF", "#0000FF")
  Bl2Rd10 <- c("#FF0000", "#F9A7B0", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#ADDFFF", "#1589FF")
  pal_fun <- colorQuantile(palette = Bl2Rd10, NULL, n = 10)
  
  # popup # percentile(score_vec),
  #percentile <- ecdf(polyg_subset$mean_score)
  p_popup <- paste0("Efficiency Score: ", round(variable, 2),
                    "<br><br>Accessibility Score: <strong>", round(polyg_subset$mean_score, 2),"</strong>",
                    "<br>Needs Score:  <strong>", round((1/3)*(polyg_subset$pop+ polyg_subset$trafficScore+ polyg_subset$amn_dens), 2),
                    "<br><br>Population Score: <strong>",  round(polyg_subset$pop, 2),"</strong>",
                    "<br>Traffic score: <strong>",  round(polyg_subset$trafficScore, 2),"</strong>",
                    "<br>Facility density: <strong>",  round(polyg_subset$amn_dens, 2),"</strong>",
                    "<br><br>Block ID: ", polyg_subset$DBUID)
  
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
              title = glue('{amn_name} Map'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}
