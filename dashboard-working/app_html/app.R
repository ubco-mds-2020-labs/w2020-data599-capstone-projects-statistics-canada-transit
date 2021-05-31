library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library(tidyverse)
library(shiny)
library(glue)

path <- "../../code/Visualizations/R/HTML Maps/"

ui <- navbarPage("Transit Accessibility Dashboard", id="null",
    
    tabPanel("Interactive map",
        includeHTML("../../code/Visualizations/R/HTML Maps/Nearest Theatre Performance And Concert Hall.html"), # this code produces the HTML output
        div(class="outer",
            tags$head(
                # Include personalized CSS
                includeCSS("styles.css"),
                includeScript("gomap.js")
            ),
            htmlOutput("inc"), # this code here does NOT produce the HTML output
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          ## Create panel
                          h2("BLOCKS explorer"),
                          
                          #Create dropdowns
                          selectInput(inputId = "type", label = "Amenity Type", choices = c("all", "gallery", "library or archives", "museum","theatre performance and concert hall"), selected = 'all'),
                          selectInput(inputId = "weight", label =  "Weighted", choices= c('no', 'yes'), selected = 'no'),
                          selectInput(inputId = "nearest_n", label =  "Nearest n amenities", choices = c('all', '1', '2', '3'), selected = "all"),
                          
                          #Plots
                          # plotOutput("histCentile", height = 200),
                          # plotOutput("scatterCollegeIncome", height = 250)
            ),
             
            # Create citation for map source
            tags$div(id="cite",
                     'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
            )
        ),
    includeHTML("../../code/Visualizations/R/HTML Maps/Nearest Theatre Performance And Concert Hall.html") # this code produces the HTML output
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reload map whenever new scoring scheme is selected
        select_html = reactive({
            
            # Create file name to select
            nearest_n <- input$nearest_n
            
            if(nearest_n == "all"){
                nearest_n_str <- "all" 
            } else if(nearest_n == 1){
                nearest_n_str <- 'nearest'
            } else if(nearest_n == 2){
                nearest_n_str <- "nearest 2"
            } else{
                nearest_n_str <- "nearest 3"
            }
            
            type < -input$type
            
            if(type == "all"){
                type_str <- "all cultural amenities"
            } else{
                type_str <- type
            }
            
            weight <- input$weight
            
            if(weight == "no"){
                weight_str <- "(Weighted)"    
            } else{
                weight_str <- ""
            }
            
            html_name <- glue("{nearest_n_str} {type_str} {weight_str}")

            return(html_name)
        })
        
    getPage<-function(){
        file_name <- select_html()
        return(includeHTML(glue("{path}{file_name}.html")))
    }
    output$inc<-renderUI({getPage()})
    
}


shinyApp(ui, server)