```{r}

score_visualizer <- function(st_obj, score_col, title) {
  
  score_vec <- st_obj[[score_col]]
  
  # colour palette
  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 5)
  
  # interactive popup
  p_popup <- paste0("<strong>Accessibility:</strong>", score_vec)
  
  # plot
  plot <- leaflet(st_obj) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(score_vec), # set fill colour with pallette fxn
      fillOpacity = 0.5, smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
    addLegend("topright",  
              pal=pal_fun, # palette function
              values=~score_vec,  # value to be passed to palette function
              title = title) # legend title
  plot
}


```

```{r}
# leaflet requires the sf object be reprojected
van_dbs_scores_st <- st_transform(van_dbs_scores_sf, crs = 4326)

# score column names
score.names <- names(van_dbs_scores_sf)[3:12]

# title names
title.vec <- score.names  %>% 
  str_replace_all('_', ' ') %>% 
  str_replace_all('  ', ' ') %>% 
  str_replace_all(' scores', '') %>% 
  str_replace_all('1', '') %>% 
  str_replace_all('3', ' 3') %>% 
  str_replace_all('destination', 'Cultural Amenities') %>% 
  str_replace_all('simulated weighted', '(weighted)') %>% 
  str_to_title() %>% 
  str_replace_all('O', 'o')


map_collection <- NULL

for (i in 1:length(score.names)) {
  temp_map <- score_visualizer(st_obj = van_dbs_scores_st,
                               score_col = score.names[i],
                               title = title.vec[i])
  
  mapshot(temp_map, url = paste0(getwd(), glue::glue("/{title.vec[i]}.html")))
  
  ma