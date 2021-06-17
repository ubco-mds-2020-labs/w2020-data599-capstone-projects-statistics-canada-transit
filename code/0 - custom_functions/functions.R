## PROJECT CUSTOM FUNCTIONS
# This file stores many base functions used for the project
# These include scoring, mapping, wrangling, and normalization functions
# Developers: Luka, Rain, Graham, Yuxuan

##############################
## NORMALIZATION FUNCTIONS
##############################

# Normalize vector to a custom range [x,y]

normalize_vec <- function(vec, x = 0.01, y = 0.99, log = FALSE) {
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

# Transit Accessibility Measure Scoring Function
# It takes the inverse of the "worst case" trip time from 1 point to the
# nearest 1, 2, 3, ... , n amenities.
#  score SUM [i..n] (1/(mean_traveltime_i + 2*std_traveltime_j) + 1/...  + 1/...   ...))

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

# Expands a grid of NA values based on the factors of missing values in a dataframe.
# This is useful if you want NA values regions to be represented on a map.
# For example, each origin ID expects 32 values, but in many cases only 8 or 16
# are present. In that case we would expand a grid using the missing factors to
# reincorporate those NA values into the frame. 

NA_grid_maker <- function(id, df, frame_type) {

  
  
  # create NA rows to append via expand.grid (creates a row for every factor combination)
  if (frame_type == 'efficiency') {

    # it doesn't really expand a grid because there are no factors! can be updated
    NA_rows <- expand.grid('fromId' = id, 'mean_score' = NA,
                           'pop' = NA, 'amn_dens' = NA,
                           'trafficScore' = NA, 'need' = NA,
                           'eff' = NA, stringsAsFactors = TRUE)
  } else {
    # get all amenities
    all_amenities <- as.character(unique(df$type))
    # get missing amenities by indexing the present amenities at each fromId
    missing_amenities <- setdiff(all_amenities, unique(df$type[df$fromId == id]))
  }
  if (frame_type == 'score') {
    
    NA_rows <- expand.grid('fromId' = id, 'type' = missing_amenities,
                            'weight' = as.character(unique(df$weight)),
                            'nearest_n' = as.character(unique(df$nearest_n)),
                            'score' = NA, stringsAsFactors = TRUE)

  } else if (frame_type == 'isochrone') {
    
    NA_rows <- expand.grid('fromId' = id, 'type' = missing_amenities,
                           'time_groups' = NA,  stringsAsFactors = TRUE)
  } 

  NA_rows

}

# Function that calls on the grid expander and performs the filling or addition
# of NA valued rows. Custom idx is for adding values not in the original frame,
# otherwise values that dont meet expected occurence are filled.
# frame_types can be c('score', 'isochrone', 'efficiency')

NA_table_filler <- function(df, custom_idx = NULL, frame_type) {
  
  # count each fromId occurence
  fromId_counts <- df %>% group_by(fromId) %>% mutate(n = n())
  x <- max(fromId_counts$n)

  if (is.null(custom_idx)) {
    # create a fromId array using Ids that don't meet the [x] count requirement
    id_arr <- array(unique(fromId_counts[fromId_counts$n < x, ]$fromId))
  } else {
    # use a custom index of fromIds (for example those missing from the frame)
    id_arr <- custom_idx
  }

  # get the NA filler rows
  filler_rows <- rbindlist(apply(id_arr, MARGIN = 1, FUN = NA_grid_maker, df = df, frame_type = frame_type))

  # append to input dataframe sort them
  df <- rbindlist(list(df, filler_rows), use.names = TRUE) 
  
  if (frame_type == 'score') {
    df <- df %>% arrange(fromId, type, nearest_n, weight)
  } else if (frame_type == 'isochrone'){
    df <- df %>% arrange(fromId, type)
  } else if (frame_type == 'efficiency') {
    df <- df %>% arrange(fromId)
  }
  
  df
}


##############################
## RUNNING AVERAGE FUNCTIONS
##############################

## CHECK1 i'm not sure if 0.0675 is really 500m, it seems more like it's 7km??
## CHECK2 i'm not sure if the function should be calling on a global object. Can we make it more "functional" by nature?

# Function that calculates the mean traffic count within 500m of each block

db_trafic <- function(row){
  mean(filter(traffic_data,
                trafic_data$LATITUDE <= (as.numeric(row["lat"])+0.0675) & 
                trafic_data$LATITUDE >= (as.numeric(row["lat"])-0.0675) & 
                trafic_data$LONGITUDE <= (as.numeric(row["lon"])+0.0675) & 
                trafic_data$LONGITUDE >= (as.numeric(row["lon"])-0.0675))$TrafficCount) 
}


# Iterate Through each row
# Get the most recent traffic survey for each row

is_NA <- function(row, col = 21) {

  if (is.na(row[col+3]) & col>0) {
    is_NA(row = row, col = col-1)
  } else if (col == 0 ){
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


# Mapping function for score tables

map_maker_scores <- function(data, bus_data, amenity, weight, nearest_n, add_stop, output_dir, view_map = FALSE) {
  
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
  stop_popup<-paste0("STOP ID: <strong>",bus_data$stop_id,"</strong>",
                     "<br>STOP Name: <strong>",bus_data$stop_name)
  
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
  if(add_stop==TRUE & view_map==FALSE){
    map%>%addCircles(data=bus_data,~longitude, ~latitude, weight = 1, radius=5,
                     color="#0073B2", stroke = TRUE, fillOpacity = 0.8,popup =stop_popup)->map
    
    file_name <- glue('{amn_name} - wt({weight}) - n({str_to_upper(nearest_n)})-bus')
    print(paste('Current Map:', file_name))
    
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}



# Mapping function for isochrone tables

map_maker_isochrone <- function(data, bus_data, amenity, add_bus, output_dir, view_map = FALSE) {
  
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
  stop_popup<-paste0("STOP ID: <strong>",bus_data$stop_id,"</strong>",
                     "<br>STOP Name: <strong>",bus_data$stop_name)
  
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
  if(add_stop==TRUE & view_map==FALSE){
    map%>%addCircles(data=bus_data,~longitude, ~latitude, weight = 1, radius=5,
                     color="#0073B2", stroke = TRUE, fillOpacity = 0.8,popup =stop_popup)->map
    file_name <- glue('{amn_name} Transit Isochrone with bus stops')
    print(paste('Current Map:', file_name))
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}



# Efficiency maps

map_maker_efficiency_cont <- function(data, mapTitle = "Continuous Efficiency", output_dir, view_map = FALSE) {
    
  file_name <- glue('{mapTitle} Map')
  print(paste('Current Map:', file_name))
  
  # subset info
  polyg_subset <- data
  
  # variable vector
  variable <- polyg_subset$eff
  
  # colour palette 
  Bl2Rd <- c("#FA8072", "#F9A7B0", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#ADDFFF", "#1589FF")
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
              title = glue('{mapTitle} Map'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}

map_maker_efficiency_quant <- function(data, mapTitle = "Quantitative Efficiency", output_dir, view_map = FALSE) {
  
  file_name <- glue('{mapTitle} map')
  print(paste('Current Map:', file_name))
  
  # subset info
  polyg_subset <- data
  
  # variable vector
  variable <- polyg_subset$eff
  
  # colour palette 
  Bl2Rd <- c("#FA8072", "#F9A7B0", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#ADDFFF", "#1589FF")
  pal_fun <- colorQuantile(palette = Bl2Rd10, NULL, n = 10)
  
  # popup # percentile(score_vec),
  #percentile <- ecdf(polyg_subset$mean_score)
  p_popup <- paste0("Efficiency Score: ", round(variable, 2),
                    "<br><br>Accessibility Score: <strong>", round(polyg_subset$mean_score, 2),"</strong>",
                    "<br>Needs Score:  <strong>", round((1/3)*(polyg_subset$pop+ polyg_subset$trafficScore+ polyg_subset$amn_dens), 2),
                    "<br><br>Population Score: ",  round(polyg_subset$pop, 2),"",
                    "<br>Traffic score: ",  round(polyg_subset$trafficScore, 2),"",
                    "<br>Facility density: ",  round(polyg_subset$amn_dens, 2),"",
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
              title = glue('{mapTitle} Map'))
  
  if (view_map == TRUE) {
    return(map)
  } else {
    mapshot(map, url = glue("{getwd()}/{output_dir}/{file_name}.html"))
  }
  
}