library(shiny)
library(glue)
library(stringr)

# Factor vector names
amenity_factor <- c("Library or Archives", "Gallery", "Museum", "Theatre and Concert Hall")
weight_factor <- c('No', 'Yes')
nearest_n_factor <- c('1', '2', '3', 'ALL')

# Directory path
isomap_dir <- "/isochrone_maps"
kepmap_dir <- "/kepler_maps"
scoremap_dir <- "/score_maps"
effmap_dir <- "/efficiency_maps"
addResourcePath('isomaps', paste0(getwd(), isomap_dir)) # 'isomaps' is the name of the resource
addResourcePath('kepmap', paste0(getwd(), kepmap_dir)) # 'kepmap' is the name of the resource
addResourcePath('maps', paste0(getwd(), scoremap_dir)) # 'maps' is the name of the resource
addResourcePath('effmap', paste0(getwd(), effmap_dir)) # 'effmap' is the name of the resource


ui <- shinyUI(
    navbarPage("Transit Accessibility Dashboard",
               tabPanel("Interactive Transit Accessibility Map",
                        div(class="outer",
                            
                            # styles
                            tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                            
                            # leaflet html map
                            htmlOutput('map'),
                            
                            # options panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          fixed = TRUE, draggable = TRUE,
                                          top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("Accessibility Explorer"),
                                          selectInput(inputId = "type", label = "Amenity Type", choices = amenity_factor),
                                          selectInput(inputId = "weight", label =  "Popularity Weights", choices= weight_factor),
                                          selectInput(inputId = "nearest_n", label =  "Nearest n Amenities", choices = nearest_n_factor)),
                            
                            # citations
                           # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                           )
               ),
               
               navbarMenu("More",
                          "----",
                          "Visualizations",
                          tabPanel("Interprettable Isochrones",
                                   div(class="outer",
                                       
                                       # styles
                                       tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                                       
                                       # get the map
                                        htmlOutput('map_iso'),
                                       
                                       # options panel
                                       absolutePanel(id = "controls", class = "panel panel-default",
                                                     fixed = TRUE, draggable = TRUE,
                                                     top = 60, left = "auto", right = 20, bottom = "auto",
                                                     width = 330, height = "auto",
                                                     h2("Accessibility Explorer"),
                                                     selectInput(inputId = "type_iso", label = "Amenity Type", choices = amenity_factor)),
                                       
                                       # citations
                                      # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                                      )
                          ),
                          
                          tabPanel("Kepler Visualizations",
                            htmlOutput('kepler'),
                          ),
                          tabPanel("Network Efficiency",
                                   tabPanel("Interprettable Isochrones",
                                            div(class="outer",
                                                
                                                # styles
                                                tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                                                
                                                # get the map
                                                htmlOutput('map_eff'),
                                                
                                                # options panel
                                                absolutePanel(id = "controls", class = "panel panel-default",
                                                              fixed = TRUE, draggable = TRUE,
                                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                                              width = 330, height = "auto",
                                                              h2("Accessibility Explorer"),
                                                              selectInput(inputId = "type_eff", label = "Amenity Type", choices = amenity_factor)),
                                                
                                                # citations
                                                # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                                            )
                                   ),
                          ),
                          tabPanel("Urban Equity"),
                          "----",
                          "Learn More",
                          tabPanel("About the Project"),
                          tabPanel("Data Explorer")
               )
    )
)
    
server <- function(input, output){
    
    # get html path
    getPage <- reactive({ 
        nearest_n <- input$nearest_n
        weight <- str_to_lower(input$weight)
        amn_name <- input$type
        html_file <- glue("{amn_name} - wt({weight}) - n({nearest_n})")
        return(glue('/{html_file}.html'))
    })
    
    getPage_kep <- reactive({ 
        amn_name <- input$type_iso
        html_file <-  glue('all_type_Kepler')
        return(glue('/{html_file}.html'))
    })
    
    getPage_iso <- reactive({ 
        amn_name <- input$type_iso
        html_file <-  glue('{amn_name} Transit Isochrone')
        return(glue('/{html_file}.html'))
    })
    
    getPage_eff <- reactive({ 
        amn_name <- input$type_eff
        html_file <-  glue('{amn_name} efficiency map')
        return(glue('/{html_file}.html'))
    })
    
    # dynamic file calling
    output$map <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('maps', getPage()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling kepler map
    output$kepler <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('kepmap', getPage_kep()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling isochrone map
    output$map_iso <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('isomaps', getPage_iso()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling efficiency map
    output$map_eff <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('effmap', getPage_eff()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
}
    
shinyApp(ui, server)
