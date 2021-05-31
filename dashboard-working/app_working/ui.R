library(leaflet)


navbarPage("Transit Accessibility Dashboard", id="nav",

### Create first tab
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include personalized CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Panel settings
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        ## Create panel
        h2("BLOCKS explorer"),

        #Create dropdowns
        selectInput(inputId = "type", label = "Amenity Type", choices = c( "gallery", "library or archives", "museum","theatre/performance and concert hall")),
        selectInput(inputId = "weight", label =  "Weighted", choices= c('no', 'yes')),
        selectInput(inputId = "nearest_n", label =  "Nearest n amenities", choices = c('all', '1', '2', '3')),

        #Plots
       # plotOutput("histCentile", height = 200),
       # plotOutput("scatterCollegeIncome", height = 250)
      ),

      # Create citation for map source
      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),


  # create second Isochrone map tapb
tabPanel("Isochrone map",
         div(class="outer",
             
             tags$head(
               # Include personalized CSS
               includeCSS("styles.css"),
               includeScript("gomap.js")
             ),
             
             leafletOutput("iso_map", width="100%", height="100%"),
             
             # Panel settings
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           ## Create panel
                           h2("BLOCKS explorer"),
                           
                           #Create dropdowns
                           selectInput(inputId = "type", label = "Amenity Type", choices = c( "gallery", "library or archives", "museum","theatre/performance and concert hall")),
                           selectInput(inputId = "time", label =  "Transit Time", choices= c('15 mins', '30 mins','45 mins')),
                           selectInput(inputId = "nearest_n", label =  "Nearest n amenities", choices = c('all', '1', '2', '3')),
                           
                           #Plots
                           # plotOutput("histCentile", height = 200),
                           # plotOutput("scatterCollegeIncome", height = 250)
             ),
             
             # Create citation for map source
             tags$div(id="cite",
                      'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
             )
         )
),
  conditionalPanel("false", icon("crosshair"))
)
