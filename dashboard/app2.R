library(shiny)
library(glue)
library(stringr)

# Directory path
scoremap_dir <- "/maps/score_maps"
kepmap_dir <- "/maps/kepler_maps"
isomap_dir <- "/maps/isochrone_maps"
effmap_dir <- "/maps/efficiency_maps"
keptime_dir <- "/kepler_time"

# Add resource paths, with specific resource names
addResourcePath('map_sco', paste0(getwd(), scoremap_dir))
addResourcePath('map_kep', paste0(getwd(), kepmap_dir))
addResourcePath('map_iso', paste0(getwd(), isomap_dir)) 
addResourcePath('map_eff', paste0(getwd(), effmap_dir)) 
addResourcePath('keptime', paste0(getwd(), keptime_dir))

# Factor vector names
amenity_factor <- c("Library or Archives", "Gallery", "Museum", "Theatre and Concert Hall")
weight_factor <- c('No', 'Yes')
nearest_n_factor <- c('1', '2', '3', 'ALL')
stops <- c('No', 'Yes')
efficiency_type <- c('Continuous', 'Discrete')
day_factor <- c('Friday', 'Saturday', 'Sunday')

ui <- shinyUI(
    navbarPage("Vancouver Transit Accessibility to Cultural Amenities",
                tabPanel('About this Project'),
                navbarMenu("Visualizations",
                          "----",
                            tabPanel("Score Measures", 
                            div(class="outer",
                                tags$head(includeCSS("styles.css"), includeScript("gomap.js")), # styles
                                htmlOutput('map_sco'), # leaflet html map
                                # options panel
                                absolutePanel(id = "controls", class = "panel panel-default",
                                                fixed = TRUE, draggable = TRUE,
                                                top = 60, left = "auto", right = 20, bottom = "auto",
                                                width = 330, height = "auto",
                                                h2("Accessibility Explorer"),
                                                selectInput(inputId = "type_sco", label = "Amenity Type", choices = amenity_factor),
                                                selectInput(inputId = "weight", label =  "Amenity Weights", choices= weight_factor),
                                                selectInput(inputId = "nearest_n", label =  "Nearest n Amenities", choices = nearest_n_factor),
                                                selectInput(inputId = 'stop_sco', label = "Include Bus Stops", choices = stops)),
                                # citations
                                # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                            )),
                            tabPanel("3D Score Measures",
                            div(class="outer",
                                tags$head(includeCSS("styles.css"), includeScript("gomap.js")), # styles
                                htmlOutput('map_kep'), # kepler.gl html map
                                # options panel
                                absolutePanel(id = "controls", class = "panel panel-default",
                                            fixed = TRUE, draggable = TRUE,
                                            top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Accessibility Explorer"),
                                            selectInput(inputId = "type_kep", label = "Amenity Type", choices = amenity_factor)),
                                # citations
                                # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                            )),
                            tabPanel("Isochrone Measures",
                            div(class="outer",
                                tags$head(includeCSS("styles.css"), includeScript("gomap.js")), # styles
                                htmlOutput('map_iso'), # get the map
                                # options panel
                                absolutePanel(id = "controls", class = "panel panel-default",
                                            fixed = TRUE, draggable = TRUE,
                                            top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Accessibility Explorer"),
                                            selectInput(inputId = "type_iso", label = "Amenity Type", choices = amenity_factor),
                                            selectInput(inputId = 'stop_iso', label = "Include Bus Stops", choices = stops)),
                                # citations
                                # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                            )),
                            tabPanel("Kepler.gl Time Window",
                            div(class="outer",
                                tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                                htmlOutput('keplertime'),
                                # options panel
                                absolutePanel(id = "controls", class = "panel panel-default",
                                            fixed = TRUE, draggable = TRUE,
                                            top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Accessibility Explorer"),
                                            selectInput(inputId = "type_kep", label = "Amenity Type", choices = amenity_factor),
                                            selectInput(inputId = "day_kep", label = "Day", choices = day_factor)),
                                       
                                # citations
                                # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                            )),
                            tabPanel("Network Efficiency",
                                    div(class="outer",
                                        tags$head(includeCSS("styles.css"), includeScript("gomap.js")), # styles
                                        htmlOutput('map_eff'), # get the map
                                        # options panel
                                        absolutePanel(id = "controls", class = "panel panel-default",
                                                    fixed = TRUE, draggable = TRUE,
                                                    top = 60, left = "auto", right = 20, bottom = "auto",
                                                    width = 330, height = "auto",
                                                    h2("Accessibility Explorer"),
                                                    selectInput(inputId = 'type_eff', label = "Efficiency Type", choices = efficiency_type),
                                                    selectInput(inputId = 'stop_eff', label = "Include Bus Stops", choices = stops)),
                                        # citations
                                        # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                                    ))
                )#,
                #tabPanel('Data Explorer')
    )
)


    
server <- function(input, output){
    
    # get html path
    getScore_map <- reactive({ 
        amn_name <- input$type_sco
        weight <- str_to_lower(input$weight)
        nearest_n <- input$nearest_n
        stop <- input$stop_sco
        html_file <- glue("{amn_name} - wt({weight}) - n({nearest_n}) - stops({stop})")
        return(glue('/{html_file}.html'))
    })
    
    getKepler_map <- reactive({ 
        amn_name <- input$type_kep
        html_file <-  glue('{amn_name} Score Kepler')
        return(glue('/{html_file}.html'))
    })
    
    getIsochrone_map <- reactive({ 
        amn_name <- input$type_iso
        stop <- input$stop_iso
        html_file <-  glue('{amn_name} - isochrone - stops({stop})')
        return(glue('/{html_file}.html'))
    })
    
    getEfficiency_map <- reactive({ 
        efficiency <- input$type_eff
        stop <- input$stop_eff
        html_file <-  glue('{efficiency} Efficiency - stops({stop})')
        return(glue('/{html_file}.html'))
    })
    
    getPage_keptime <- reactive({ 
        amn_name <- input$type_kep
        day <- input$day_kep
        html_file <-  glue('{amn_name} time {day}')
        return(glue('/{html_file}.html'))
    })
    
    # dynamic file calling
    output$map_sco <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_sco', getScore_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling kepler map
    output$map_kep <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_kep', getKepler_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling isochrone map
    output$map_iso <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_iso', getIsochrone_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling efficiency map
    output$map_eff <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_eff', getEfficiency_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling kepler time window map
    output$keplertime <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('keptime', getPage_keptime()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
}
    
shinyApp(ui, server)
