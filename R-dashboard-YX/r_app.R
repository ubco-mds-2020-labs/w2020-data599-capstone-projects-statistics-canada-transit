library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library(tidyverse)


# Import MDS data
canada_dbs <- st_read("data/Census_DBs_2016_Digital_Cartographic.gml")
input <- read.csv("data//long_score.csv")
input$fromId <- as.character(input$fromId)


# keep necessary columns and rows
van_dbs <- data.frame(canada_dbs[which(canada_dbs$CMANAME == "Vancouver"), ])
clean_van_dbs <- van_dbs[, c(1, 2, 29)]
clean_van_dbs$DBUID<-as.character(clean_van_dbs$DBUID)


# join
van_dbs_scores <- left_join(clean_van_dbs, input, by = c('DBUID' = 'fromId'))
head(van_dbs_scores)

# convert back to sf object
van_dbs_scores_sf <- st_as_sf(van_dbs_scores)
van_dbs_scores_st <- st_transform(van_dbs_scores_sf,crs = 4326)
van_dbs_scores_st %>%filter(type=="gallery"& Scoring_Scheme=="score_weighted")



library(shiny)


# Define the UI
ui = fluidPage(
  
  # App title
  titlePanel("Transit Accessibility Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "Amenity_type",
                  label = "Choose the amenity type of you want to see:",
                  choices = list("Gallery" = "gallery", "Library" = "library or archives","Museum"="museum","Theatre/performance and concert hall"="theatre/performance and concert hall")),
      
      # Second input (choices depend on the choice for the first input)
      selectInput(inputId = "Score_Scheme",
                  label = "Choose the Score Scheme of you want to see:",
                  choices = list("Weighted Score"="score_weighted","Unweighted Score"="score_unweighted")),
      
    ),

    # Main panel for displaying outputs
    mainPanel(
        tabPanel("Vancouver Map", leafletOutput("map", width = "100%", height = "500px"))
  )
))



# Define the server
server = function(input, output) {
  
  # Create accessibility map
  # colour palette
  Rd2Gn <- c("#e30606", "#fd8d3c", "#ffe669", "#cdff5e", "#64ed56")
  pal_fun <- colorQuantile(palette = Rd2Gn, NULL, n = 5)
  
  p_popup <- paste0("<h5>Accessibility Score: ", round(van_dbs_scores_st$value, 4),"</h5>","<br><strong>Block ID: ", van_dbs_scores_st$DBUID,"<br><strong>Population: ", van_dbs_scores$pop,"</strong")
  factop <- function(x) {
    ifelse(is.na(x), 0, 0.6)
  }
  
  van_dbs_scores_sf
  
  output$map <- renderLeaflet({
    van_dbs_scores_st<-van_dbs_scores_st%>%filter(type==input$Amenity_type & Scoring_Scheme==input$Score_Scheme )
    leaflet(van_dbs_scores_st) %>%
      addPolygons(
        stroke = FALSE,  # remove polygon borders
        fillColor = ~pal_fun(value), # set fill colour with pallette fxn from aboc
        fillOpacity = ~factop(value), smoothFactor = 0.5, # aesthetics
        popup = p_popup) %>% # add message popup to each block
      addTiles() %>%
      setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
      addLegend("bottomleft",  # location
                pal=pal_fun,    # palette function
                values=~value,  # value to be passed to palette function
                title = "Vancouver Transit Accessibility") # legend title)
  })
}


# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)
