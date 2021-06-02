library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library(tidyverse)

# Import score data (long table)
#allblocks <- read.csv("../../data/score_sets/long_scores.csv", stringsAsFactors = TRUE)
allblocks <- read.csv("newest_long_scores.csv", stringsAsFactors = TRUE)
allblocks$fromId <- as.character(allblocks$fromId)

# Import shape file data
# keep necessary columns and rows for shp
canada_dbs <- st_read("DB_Van_CMA.shp", stringsAsFactors = FALSE)
van_dbs <- data.frame(canada_dbs[which(canada_dbs$CMANAME == "Vancouver"), ])
van_dbs$ID <- seq.int(nrow(van_dbs))
clean_van_dbs <- van_dbs[, c(1, 28, 29)]

# join data into a single dataframe
# convert back to sf object
van_dbs_scores <- left_join(clean_van_dbs, allblocks, by = c('DBUID' = 'fromId'))
van_dbs_scores_sf <- st_as_sf(van_dbs_scores)
van_dbs_scores_st <- st_transform(van_dbs_scores_sf,crs = 4326)

#Prepare data for interactive dashboard
blockdata <- allblocks[sample.int(nrow(allblocks)),]
blockdata <- blockdata[order(blockdata$score),]


function(input, output, session) {

  ## Interactive Map ###########################################
  
  # colour palette (including NA)
  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 5)
  #pal_fun <-  colorNumeric(Rd2Gn, domain = 0:1)
  
  # Reload map whenever new scoring scheme is selected
  dashboard_int = reactive({
    polyg_subset <- van_dbs_scores_st[van_dbs_scores_st$type == input$type &
                                      van_dbs_scores_st$weight == input$weight &
                                      van_dbs_scores_st$nearest_n == input$nearest_n, ]
    return(polyg_subset)
  })
  
  # Create map    
  output$map <- renderLeaflet({
    
    data <- dashboard_int()
    
    # Create popup when select a datablock
    p_popup <- paste0("<h5>Accessibility Score: ", round(data$score, 4),"</h5>","<br><strong>Block ID: ", data$DBUID,"</strong>")
    
    # Add add-ons to maps
    leaflet(data = data) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(score), # set fill colour with pallette fxn from aboc
      fillOpacity = 0.6, smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
      setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
    addLegend("bottomleft",  # location
              pal=pal_fun,    # palette function
              values=~score,  # value to be passed to palette function
              title = "Vancouver Gallery Transit Accessibility") # legend title
  })
  
}
