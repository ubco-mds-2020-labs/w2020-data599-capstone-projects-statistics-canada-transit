library(shiny)
library(shinybusy)
library(glue)
library(stringr)

# for data table page
library(DT)
library(ggpubr)
library(tidyverse)
library(readr)
library(ggplot2)

# functions
#source('functions.R')

# for unpuervised learning page
library(dplyr)
library(cluster)
library(FactoMineR)
library(shinyalert)
library(factoextra)

# corr plot
library("cowplot")
library("corrplot")

#  import data
all_ams <- read.csv("datatable/all_data.csv")[,-c(1,2)]  # ams = accessibility measures
sumstat_df <- read_csv("datatable/summary_statistics_by_city.csv")[,-1]
df_pca <- read_csv("datatable/pca_data_1.csv")
df_pca <- data.frame(column_to_rownames(df_pca, var = "NAME"))
df.num <- df_pca%>%select(where(is.numeric),-avg_score)
colnames(df.num) <- c("SCORE","POPULATION","AMENITY","BUS STOPS","BUS FREQ","INDEX","TRANSIT TIME")


# Directory path
scoremap_dir <- "/maps/score_maps"
kepmap_dir <- "/maps/kepler_maps/general"
isomap_dir <- "/maps/isochrone_maps"
effmap_dir <- "/maps/efficiency_maps"
kepmap_dir_time_window <- "/maps/kepler_maps/time_window"
compare_dir <- "/maps/kepler_maps/compare"

# Add resource paths, with specific resource names
addResourcePath('map_sco', paste0(getwd(), scoremap_dir))
addResourcePath('map_kep', paste0(getwd(), kepmap_dir))
addResourcePath('map_iso', paste0(getwd(), isomap_dir)) 
addResourcePath('map_eff', paste0(getwd(), effmap_dir)) 
addResourcePath('kep_time', paste0(getwd(), kepmap_dir_time_window))
addResourcePath('kep_com', paste0(getwd(), compare_dir))

# Factor vector names
amenity_factor <- c("Library or Archives", "Gallery", "Museum", "Theatre and Concert Hall")
weight_factor <- c('No', 'Yes')
nearest_n_factor <- c('1', '2', '3', 'ALL')
stops <- c('No', 'Yes')
efficiency_type <- c('Continuous', 'Discrete')
day_factor <- c('Friday', 'Saturday', 'Sunday')


ui <- shinyUI(
   # add_busy_spinner(spin = "fading-circle"),
    navbarPage("Vancouver Transit Accessibility to Cultural Amenities",

                navbarMenu("Leaflet Accessibility Visualizations", 
                           "----",
                           tabPanel("Score Measures", 
                                    div(class="outer",
                                        tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")), # styles
                                        htmlOutput('map_sco'), # leaflet html map
                                        absolutePanel(id = "controls", class = "panel panel-default",
                                                      fixed = TRUE, draggable = TRUE,
                                                      top = 60, left = "auto", right = 20, bottom = "auto",
                                                      width = 360, height = "auto",
                                                      h2("Accessibility Explorer"),
                                                      h4("Selected map score measure:"),
                                                      h5("Based on worst case scenario for  transit time (avg time / 2 SD). A higher score corresponds to a lower transit time"),
                                                      h4(),
                                                      selectInput(inputId = "type_sco", label = "Amenity Type", choices = amenity_factor),
                                                      selectInput(inputId = "weight", label =  "Amenity Weights", choices= weight_factor),
                                                      selectInput(inputId = "nearest_n", label =  "Nearest n Amenities", choices = nearest_n_factor),
                                                      selectInput(inputId = 'stop_sco', label = "Include Bus Stops", choices = stops)
                                                      
                                        ),
                                    )),
                           tabPanel("Isochrone Measures",
                                    div(class="outer",
                                        tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")), # styles
                                        htmlOutput('map_iso'), # get the map
                                        absolutePanel(id = "controls", class = "panel panel-default",
                                                      fixed = TRUE, draggable = TRUE,
                                                      top = 60, left = "auto", right = 20, bottom = "auto",
                                                      width = 360, height = "auto",
                                                      h2("Accessibility Explorer"),
                                                      h4("Selected map score measure:"),
                                                      h5("Maximing time to get to the nearest amenity."),
                                                      h4(),
                                                      selectInput(inputId = "type_iso", label = "Amenity Type", choices = amenity_factor),
                                                      selectInput(inputId = 'stop_iso', label = "Include Bus Stops", choices = stops)),
                                    )),
                           tabPanel("Network Efficiency",
                                    div(class="outer",
                                        tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")), # styles
                                        htmlOutput('map_eff'), # get the map
                                        # options panel
                                        absolutePanel(id = "controls", class = "panel panel-default",
                                                      fixed = TRUE, draggable = TRUE,
                                                      top = 60, left = "auto", right = 20, bottom = "auto",
                                                      width = 360, height = "auto",
                                                      h2("Accessibility Explorer"),
                                                      h4("Selected map score measure:"),
                                                      h5("Scores based on the difference between the accessibility and the transit needs"),
                                                      h4(),
                                                      selectInput(inputId = 'type_eff', label = "Efficiency Type", choices = efficiency_type),
                                                      selectInput(inputId = 'stop_eff', label = "Include Bus Stops", choices = stops)),
                                    ))
                ),

               navbarMenu("Kepler Accessibility Visualizations",
                          "----",
                          tabPanel("Kepler Score Measures",
                                   div(class="outer",
                                       tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")), # styles
                                       htmlOutput('map_kep'), # kepler.gl html map
                                       absolutePanel(id = "controls", class = "panel panel-default",
                                                     fixed = TRUE, draggable = TRUE,
                                                     top = 60, left = "auto", right = 20, bottom = "auto",
                                                     width = 360, height = "auto",
                                                     h2("Accessibility Explorer"),
                                                     h4("Selected map score measure:"),
                                                     h5("3D map based on worst case scenario for  transit time (avg time / 2 SD). A higher score corresponds to a lower transit time"),
                                                     h4(),
                                                     selectInput(inputId = "type_kep", label = "Amenity Type", choices = amenity_factor)),
                                   )),
                          tabPanel("Comparison",
                                   div(class="outer",
                                       tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")), # styles
                                       htmlOutput('kep_com'), # kepler.gl html map
                                       absolutePanel(id = "controls", class = "panel panel-default",
                                                     fixed = TRUE, draggable = TRUE,
                                                     top = 60, left = "auto", right = 20, bottom = "auto",
                                                     width = 360, height = "auto",
                                                     h2("Accessibility Explorer"),
                                                     h4("Selected map score measure:"),
                                                     h5("3D map comparison based on worst case scenario for  transit time (avg time / 2 SD). A higher score corresponds to a lower transit time"),
                                                     h4(),
                                                     selectInput(inputId = "type_com", label = "Amenity Type", choices = amenity_factor)),
                                   ))
                          
                          #tabPanel("3D Time Window",
                          #div(class="outer",
                          #    tags$head(includeCSS("styles/styles.css"), includeScript("styles/gomap.js")),# styles
                          #    htmlOutput('keplertime'),
                          #    absolutePanel(id = "controls", class = "panel panel-default",
                          #                fixed = TRUE, draggable = TRUE,
                          #                top = 60, left = "auto", right = 20, bottom = "auto",
                          #                width = 360, height = "auto",
                          #                h2("Accessibility Explorer"),
                          #                h4("Selected map score measure:"),
                          #                h5("3D map based on the maximing time to get to the nearest amenity."),
                          #                h4(),
                          #                selectInput(inputId = "type_kep_time", label = "Amenity Type", choices = amenity_factor),
                          #                selectInput(inputId = "day_kep", label = "Day", choices = day_factor)),
                          #))
                          
               ),
               tabPanel("Unsupervised Analysis",
                        tags$div(
                            sidebarPanel(
                                selectInput("var","Select Variables:",
                                            choices = colnames(df.num),
                                            multiple = T,
                                            selected=colnames(df.num)),
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Scree Plot",
                                             plotOutput("plot_scree")),
                                    tabPanel("Correlation Plot",
                                             plotOutput("plot_cor")),
                                    tabPanel(" Contributions Plot",
                                             plotOutput("plot_con")),
                                    tabPanel("Individual Plot",
                                             plotOutput("plot_ind")),
                                    tabPanel("Biplot",
                                             plotOutput("plot_bi")),
                                    tabPanel("Clustering",
                                             plotOutput("plot_cluster"))
                                )))
               ),
               tabPanel("Data Explorer",
                        tabPanel("summary_statistics", DT::dataTableOutput("summary_table")),
                        
                        # Create a new Row in the UI for selectInputs
                        fluidRow(
                            column(4,
                                    selectInput("weights",
                                                "Weights:",
                                                choices=c("Yes"="yes","No"="no"),
                                                selected = "Yes")
                            ),
                            column(4,
                                    selectInput(inputId="nearest",
                                                label="Access to:",
                                                choices=c("All Amenities" = "avg_time_to_any_amenity",
                                                          "Nearest Amenity" = "time_to_nearest_amenity"),
                                                selected = "All Amenities")
                            )
                        ),
                        plotOutput("subdivision_violin_plot", click = "plot_click")
               ),
               tabPanel('About this Project',
                        tags$div(
                            tags$h2("Welcome!"),
                            "The dashboard serves as a supplement tool  
                    for our main project, A High Performing, 
                    Scalable Model forComputing and Visualizing Public Transit Accessibility,A Case Study on Cultural
                    and Art Amenities in Metro Vancouver.",
                            tags$br(),tags$br(),tags$h3("About the Dashbboard"),
                            "This dashboard is used to visualize transit network accessibility
                    in the Greater Vancouver area.
                    The main four tabs are Scores , Kepler.gl  ,Isochrones , Network Efficiency, 
                    the main purpose for those 4 tabs is to visualize  the transit accessibility 
                    using different measurements and metrics. The Score map uses the quantiles of
                    the accessibility raw score to reflect the transit access in the 
                    Greater Vancouver area, based on amenity type, popularity weights and 
                    the nearest n amenity.   The Isochrone map uses the actual transit time 
                    to visualize the transit access based on user selected amenity type. 
                    The Network efficiency map is mainly for comparing the efficiency score 
                    among subdivisions, an efficiency score was created by comparing the amount
                    of transit being offered to the needs of each block . 
                    Therefore blocks with a very high accessibility to needs ratio will have
                    a very high efficiency score, indicating an excess accessibility. T
                    he Kepler.gl is designed for visualizing large -scale geolocation data,
                    which  has been built on top of Mapbox and deck.gl, we integrate the kepler.gl
                    in our dashboard to compare the changes of transit time from each block 
                    to the nearest animenty of  selected.",
                            
                            tags$br(),tags$br(),tags$h3("About the Project"),
                            tags$h5("A High Performing, 
                    Scalable Model forComputing and Visualizing Public Transit Accessibility,A Case Study on Cultural
                    and Art Amenities in Metro Vancouver."),
                            tags$br(),tags$h4("Introduction"),
                            "Transportation network analysis is fundamental to urban planning 
                    for it determines how resources are distributed across a population. 
                    Resources come in the form of amenities such as grocery stores, schools,
                    parks, and hospitals. Our client, Statistics Canada produces data to better 
                    understand Canadaâs population, resources, economy, society, and culture.
                    They have previously developed network accessibility measures based on distance 
                    of driving, and walking to compute proximity scores for various types of amenities.",
                            
                            tags$br(),tags$h4("Problem"),
                            "Accessibility measures based on time using transit have not yet been
                    incorporated into proximity scores due to its multi-modal complexity 
                    and computational intensity. In 2016, 22.3% of Canadians depended on 
                    public transit in large cities; thus, incorporating transit accessibility
                    measures is paramount to not under-represent large segments of the population 
                    which can inevitably worsen pre-existing inequalities in the urban landscape.",
                            
                            tags$br(),tags$br(),tags$h4("Object"),
                            "The aim of this project was to establish a first iteration of an open source 
                    scalable framework for data collection and analysis of transit accessibility measures. 
                    We validated our framework on Vancouver, raising the question of, 
                    âHow accessible are Vancouverâs cultural amenities (libraries, museums, 
                    art galleries, and theatres) using the current transit system?â",
                            
                            tags$br(),tags$br(),tags$h4("Methodology/Results:"),
                            "To address the computational intensity of multimodal shortest path routing, we use Conveyalâs R5 
                    realistic routing algorithm available in R as r5r. It allows us to compute over 5.3 million transit
                    routes repeatedly, 360 times in a day over 3 days, in just a matter of one hour. The travel
                    time matrix was then used to develop three accessibility measures: one based on time, one on
                    scores, and one on percentiles which were visualized with Leaflet and Kepler.gl and embedded 
                    in an R shiny dashboard. ",
                            
                            tags$br(),tags$br(),tags$h4("Conclusion:"),
                            "This project provides a high performing and scalable framework for producing three unique 
                    transit accessibility measures for network analysis using Greater Vancouver as an initial 
                    use-case scenario. The frameworks can be further developed and adopted by urban developers
                    to ensure equitable, sustainable, and optimal urban design for years to come.",
                            
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and more detailed information are available at ",
                            tags$a(href="https://github.com/ubco-mds-2020-labs/w2020-data599-capstone-projects-statistics-canada-transit", "Github."),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Graham Kerford,Luka Vukovic,Yuxuan Cui,Rain Shen",tags$br(),
                            "Computer Science and Statistics",tags$br(),
                            "Faculty of Science,University of British Columbia",tags$br(),
                            tags$img(src = "logo.png", width = "550px", height = "200px")
                        ))
    )
)

# show_modal_spinner(
#spin = "cube-grid",
#color = "firebrick",
#text = "Please wait..."
#)
#remove_modal_spinner()

server <- function(input, output){

    # get html path
    getScore_map <- reactive({ 
        #show_modal_spinner() # show the modal window
        #remove_modal_spinner() # show the modal window
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

    getKepler_time <- reactive({ 
        amn_name <- input$type_kep_time
        day <- input$day_kep
        html_file <-  glue('{amn_name} time {day}')
        return(glue('/{html_file}.html'))
    })
    
    getKepler_com <- reactive({ 
        amn_name <- input$type_com
        html_file <-  glue('{amn_name} compare')
        return(glue('/{html_file}.html'))
    })

    # this is where the resource path names are used
    # dynamic file calling
    output$map_sco <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_sco', getScore_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%', height='100%')
    })
    
    # dynamic file calling kepler map
    output$map_kep <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_kep', getKepler_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%', height='100%')
    })
    
    # dynamic file calling isochrone map
    output$map_iso <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_iso', getIsochrone_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%', height='100%')
    })
    
    # dynamic file calling efficiency map
    output$map_eff <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('map_eff', getEfficiency_map()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%', height='100%') 
    })

    # dynamic file calling kepler time window map
    output$keplertime <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('kep_time', getKepler_time()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%', height='100%')
    })
    
   
    
    output$kep_com <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('kep_com', getKepler_com()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    # select the data table 
    output$summary_table = DT::renderDataTable({
        sumstat_df
    })
    
    # plot based on the selected row shows that  total dissemination blocks
    output$subdivision_violin_plot <- renderPlot({
        
        # user selected row indexing
        rows_selected = input$summary_table_rows_selected
        
        # keep subdivisions in selected rows
        cities_to_keep <- sumstat_df[rows_selected, 1]$subdiv
        filtered_ams <- all_ams %>%
            filter(subdiv %in% cities_to_keep & weight == input$weights)
        
        # ordered legend 
        legend_ord_score <- levels(with(filtered_ams,
                                        reorder(factor(subdiv), -avg_score_to_nearest_amenity, na.rm = TRUE)))
        
        score_plot <- filtered_ams %>% 
            ggplot(aes(y = reorder(factor(subdiv), avg_score_to_nearest_amenity, na.rm = TRUE),
                       x = avg_score_to_nearest_amenity)) +
            geom_violin(aes(fill = subdiv), scale = 'width', alpha = 0.4, draw_quantiles = c(0.5), size = 0.5) + 
            scale_fill_discrete(breaks = legend_ord_score) +
            scale_x_continuous("Average Accessibility Score",limits = c(0, 0.3), breaks=c(0,0.1,0.2,0.3)) +
            guides(fill= guide_legend(title = 'Subdivision')) +
            theme_minimal() +
            theme(aspect.ratio = 1,
                  text = element_text(size=20),
                  panel.grid.major.x = element_line(colour="lightgray", size=0.05),
                  panel.grid.major.y = element_line(colour="lightgray", size=0.05),
                  panel.grid.minor.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) 
        
        
        # change selected column name so it can be called as object in ggplot
        sub <- filtered_ams %>% select(subdiv, input$nearest)
        names(sub)[names(sub) == input$nearest] <- "selected_column"
        
        legend_ord_time <- levels(with(sub,
                                       reorder(factor(subdiv), selected_column, na.rm = TRUE)))
        
        time_plot <- sub %>%
            ggplot(aes(y = reorder(factor(subdiv), -selected_column, na.rm = TRUE), 
                       x = selected_column)) +
            geom_violin(aes(fill = subdiv), scale = 'width', alpha = 0.4, draw_quantiles = c(0.5), size = 0.5) +
            scale_fill_discrete(breaks = legend_ord_time) +
            scale_x_continuous("Average Time in Minutes") +
            guides(fill = guide_legend(title = 'Subdivision')) +
            theme_minimal() +
            theme(aspect.ratio = 1,
                  text = element_text(size=20),
                  panel.grid.major.x = element_line(colour="lightgray", size=0.05),
                  panel.grid.major.y = element_line(colour="lightgray", size=0.05),
                  panel.grid.minor.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
        
        ggarrange(score_plot, time_plot)
    })
    
    # scree
    output$plot_scree<- renderPlot({
        df_1<-df.num%>%select(input$var)
        res.pca <- prcomp(na.omit(df_1), scale = T)
        #res.pca<-prcomp(df.st,scale = T)
        eig.val <- get_eigenvalue(res.pca)
        fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 70))
    })
    
    #cor plot
    output$plot_cor<- renderPlot({
        df_1<-df.num%>%select(input$var)
        res.pca <- prcomp(na.omit(df_1), scale = T)
        var <- get_pca_var(res.pca)
        corrplot(var$cos2, is.corr=FALSE)
    })
    # contribution plot
    output$plot_con<- renderPlot({
        df_1<-df.num%>%select(input$var)
        res.pca <- prcomp(na.omit(df_1), scale = T)
        # Contributions of variables to PC1
        p1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
        # Contributions of variables to PC2
        p2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
        
        plot_grid(p1, p2, labels = "AUTO")
    })
    #ind plot
    output$plot_ind<- renderPlot({
        df_1<-df.num%>%select(input$var)
        res.pca <- prcomp(na.omit(df_1), scale = T)
        fviz_pca_ind(res.pca,
                     col.ind = "cos2", # Color by the quality of representation
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = T     # Avoid text overlapping
        ) +xlim(-9,6)+ylim(-2,2)
    })
    #bi plot plot
    output$plot_bi<- renderPlot({
        df_1<-df.num%>%select(input$var)
        res.pca <- prcomp(na.omit(df_1), scale = T)
        fviz_pca_biplot(res.pca, repel = TRUE, select.var = list(contrib =7),
                        geom = c("text","point"),
                        col.var = "#2E9FDF", # Variables color
                        col.ind = "#696969"  # Individuals color
        )
    })
    #clusteirn
    output$plot_cluster<- renderPlot({
        df_1<-df.num%>%select(input$var)
        df_1<- scale(df_1)
        # Compute k-means using 4 clusters
        set.seed(123)
        km.res <- kmeans(df_1, 4, nstart = 25)
        # Plot the k-means clustering
        fviz_cluster(km.res, df_1)+theme_minimal()
        
    })
    
}
    
shinyApp(ui, server)
