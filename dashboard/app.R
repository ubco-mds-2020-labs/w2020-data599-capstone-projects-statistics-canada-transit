library(shiny)
library(glue)
library(stringr)

# Factor vector names
amenity_factor <- c("Library or Archives", "Gallery", "Museum", "Theatre and Concert Hall")
weight_factor <- c('No', 'Yes')
nearest_n_factor <- c('1', '2', '3', 'ALL')

# Directory path
map_dir <- "/Upd_HTML_Maps"      #"../../data/Upd_HTML_Maps/"
addResourcePath('maps', paste0(getwd(), map_dir)) # 'maps' is the name of the resource

ui <- shinyUI(
    navbarPage("Transit Accessibility Dashboard",
               tabPanel("Interactive Transit Accessibility Map",
                        div(class="outer",
                            
                            # styles
                            tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                            
                            # view path - test string
                            #mainPanel(br(), br(), textOutput('string_path')),
                            
                            # options panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          fixed = TRUE, draggable = TRUE,
                                          top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("Accessibility Explorer"),
                                          selectInput(inputId = "type", label = "Amenity Type", choices = amenity_factor),
                                          selectInput(inputId = "weight", label =  "Popularity Weights", choices= weight_factor),
                                          selectInput(inputId = "nearest_n", label =  "Nearest n Amenities", choices = nearest_n_factor)),
                             
                            # leaflet html map
                            htmlOutput('map'),
                            
                            # citations
                            tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).'))
               ),
               
               navbarMenu("More",
                          "----",
                          "Visualizations",
                          tabPanel("Interprettable Isochrones"),
                          tabPanel("Kepler Visualizations",
                            htmlOutput('kepler'),
                          ),
                          tabPanel("Network Efficiency"),
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
        html_file <- glue("{amn_name} Transit Accessibility - Weighted ({weight}) - Nearest Amenities ({nearest_n})")
        return(glue('/{html_file}.html'))
    })
    
    # dynamic file calling
    output$map <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('maps', getPage()),
                    width='100%',
                    height='1250') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # dynamic file calling
    output$kepler <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('maps', "/Vancouver_Kepler.html"),
                    width='100%',
                    height='1250') # dynamic height (100%) doesn't work so I set it manually
    })
}
    
shinyApp(ui, server)