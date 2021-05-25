library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library(tidyverse)


# Import MDS data
canada_dbs <- st_read("../../code/Visualizations/census_2016_digital_DBS_gml_cartographic/ldb_000b16g_e.gml")


# keep necessary columns and rows
van_dbs <- data.frame(canada_dbs[which(canada_dbs$CMANAME == "Vancouver"), ])
clean_van_dbs <- van_dbs[, c(1, 2, 29)]

# score data
#nearest_gallery_scores <- read.csv("../../data/score_sets/nearest1_gallery_scores.csv")
#nearest_gallery_scores$fromId <- as.character(nearest_gallery_scores$fromId)

input <- read.csv("../../data/score_sets/vancouver_db_details.csv")
input$fromId <- as.character(input$id)

# join
van_dbs_scores <- left_join(clean_van_dbs, input, by = c('DBUID' = 'fromId'))
head(van_dbs_scores)

# convert back to sf object
van_dbs_scores_sf <- st_as_sf(van_dbs_scores)
van_dbs_scores_st <- st_transform(van_dbs_scores_sf,crs = 4326)

zipdata <- allblocks[sample.int(nrow(allblocks)),]
zipdata$pop <- as.numeric(zipdata$pop)
zipdata <- zipdata[order(zipdata$score),]



function(input, output, session) {

  ## Interactive Map ###########################################

  # Create accessibility map
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  p_popup <- paste0("<h5>Accessibility Score: ", round(van_dbs_scores_st$score, 4),"</h5>","<br><strong>Block ID: ", van_dbs_scores_st$DBUID,"<br><strong>Population: ", van_dbs_scores$pop,"</strong")
  factop <- function(x) {
    ifelse(is.na(x), 0, 0.6)
  }
  
  output$map <- renderLeaflet({
    leaflet(van_dbs_scores_st) %>%
    addPolygons(
      stroke = FALSE,  # remove polygon borders
      fillColor = ~pal_fun(score), # set fill colour with pallette fxn from aboc
      fillOpacity = ~factop(score), smoothFactor = 0.5, # aesthetics
      popup = p_popup) %>% # add message popup to each block
    addTiles() %>%
      setView(lng = -122.8, lat = 49.2, zoom = 11) %>%
    addLegend("bottomleft",  # location
              pal=pal_fun,    # palette function
              values=~score,  # value to be passed to palette function
              title = "Vancouver Gallery Transit Accessibility") # legend title
  })

  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allblocks$score, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$score,
      breaks = centileBreaks,
      main = "Average block score (visible blocks)",
      xlab = "Percentile",
      xlim = range(allblocks$score),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(pop ~ score, data = zipsInBounds(), xlim = range(allblocks$score), ylim = range(allblocks$pop)))
  })

  # Show a popup at the given location
#  showZipcodePopup <- function(id, lat, lng) {
#    selectedZip <- allblocks[allblocks$id == id,]
#    content <- as.character(tagList(
#      tags$h4("Score:", as.integer(selectedZip$id)),
      #tags$strong(HTML(sprintf("%s, %s %s",
      #                         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      #))), tags$br(),
     # sprintf("Median household income: %s", selectedZip$pop), tags$br(),
      #sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
     # sprintf("Adult population: %s", selectedZip$pop)
#    ))
#    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
#  }
  
  # When map is clicked, show a popup with city info
#  observe({
#    leafletProxy("map") %>% clearPopups()
#    event <- input$map_shape_click
#    if (is.null(event))
#      return()
#    
 #   isolate({
 #     showZipcodePopup(event$id, event$lat, event$lng)
 #   })
#  })

  ## Data Explorer ###########################################

#  observe({
#    cities <- if (is.null(input$states)) character(0) else {
#      filter(cleantable, State %in% input$states) %>%
#        `$`('City') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$cities[input$cities %in% cities])
#    updateSelectizeInput(session, "cities", choices = cities,
#      selected = stillSelected, server = TRUE)
#  })

#  observe({
#    zipcodes <- if (is.null(input$states)) character(0) else {
#      cleantable %>%
#        filter(State %in% input$states,
#          is.null(input$cities) | City %in% input$cities) %>%
#        `$`('Zipcode') %>%
#        unique() %>%
#        sort()
#    }
#    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
#      selected = stillSelected, server = TRUE)
#  })

#  observe({
#    if (is.null(input$goto))
#      return()
#    isolate({
#      map <- leafletProxy("map")
#      map %>% clearPopups()
#      dist <- 0.5
#      zip <- input$goto$zip
#      lat <- input$goto$lat
#      lng <- input$goto$lng
#      showZipcodePopup(zip, lat, lng)
#      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#    })
#  })

#  output$ziptable <- DT::renderDataTable({
#    df <- cleantable %>%
#      filter(
#        Score >= input$minScore,
#        Score <= input$maxScore,
#        is.null(input$states) | State %in% input$states,
#        is.null(input$cities) | City %in% input$cities,
#        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#      ) %>%
#      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#    action <- DT::dataTableAjax(session, df, outputId = "ziptable")

#    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#  })
}
