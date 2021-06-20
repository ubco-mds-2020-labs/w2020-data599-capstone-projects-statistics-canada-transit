library(shiny)
library(glue)
library(stringr)
library(DT)
library(ggpubr)
library(tidyverse)
library(readr)
library(ggplot2)
library(cluster)
library(FactoMineR)
library(shinyalert)
library(factoextra)
# Factor vector names
amenity_factor <- c("Library or Archives", "Gallery", "Museum", "Theatre and Concert Hall")
weight_factor <- c('No', 'Yes')
nearest_n_factor <- c('1', '2', '3', 'ALL')
day_factor <- c('Friday', 'Saturday', 'Sunday')

# Directory path
isomap_dir <- "/isochrone_maps"
kepmap_dir <- "/kepler_maps"
scoremap_dir <- "/score_maps"
effmap_dir <- "/efficiency_maps"
keptime_dir <- "/kepler_time"
addResourcePath('isomaps', paste0(getwd(), isomap_dir)) # 'isomaps' is the name of the resource
addResourcePath('kepmap', paste0(getwd(), kepmap_dir)) # 'kepmap' is the name of the resource
addResourcePath('maps', paste0(getwd(), scoremap_dir)) # 'maps' is the name of the resource
addResourcePath('effmap', paste0(getwd(), effmap_dir)) # 'effmap' is the name of the resource
addResourcePath('keptime', paste0(getwd(), keptime_dir)) # 'keptime' is the name of the resource

##  import data
all_df = read.csv("datatable/all_data.csv")
all_df[,-1]->all_df
sumstat_df<-read_csv("datatable/summary_statistics_by_city.csv")
sumstat_df[,-1]->sumstat_df

## PCA data
df_pca<-read_csv("datatable/pca_data_1.csv")
df_pca<-data.frame(column_to_rownames(df_pca, var = "NAME"))
df.num<-df_pca%>%select(where(is.numeric))

df.num<-df.num[,-8]
colnames(df.num)<-c("SCORE","POPULATION","AMENITY","BUS STOPS","BUS FREQ","INDEX","TRANSIT TIME")
##

##
ui <- shinyUI(
    navbarPage("Transit Accessibility Dashboard",
               tabPanel("Scores",
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
               
               tabPanel("Kepler.gl",
                        div(class="outer",
                            
                            # styles
                            tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                            
                            # kepler.gl html map
                            htmlOutput('kepler'),
                            
                            # options panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          fixed = TRUE, draggable = TRUE,
                                          top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("Accessibility Explorer"),
                                          selectInput(inputId = "type_kep", label = "Amenity Type", choices = amenity_factor)),
                            
                            # citations
                            # tags$div(id="cite", 'Data compiled for ', tags$em('Citation Here'), ' by Author (Publisher, Year).')
                        )
               ),
               
               tabPanel("Isochrones",
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
               
               tabPanel("Kepler.gl Time Window",
                        div(class="outer",
                            
                            # styles
                            tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
                            
                            # kepler.gl html map
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
                        )
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
               
               
               navbarMenu("More",
                          "----",
                          "Visualizations",
                          
                          tabPanel("Unsupervised Analysis",
                                   tags$div(
                                       sidebarPanel(
                                           #numericInput("npc", "Numer of Principal Components", 2),
                                           selectInput("var","Select Variables:",
                                                       choices = colnames(df.num),
                                                       multiple = T,
                                                      selected=colnames(df.num)),
                                       ),
                                       mainPanel(
                                                 tabsetPanel(
                                                  tabPanel("Scree Plot",
                                                           plotOutput("plot_scree")
                                                           ),
                                                  
                                                  tabPanel("Correlation Plot",
                                                           plotOutput("plot_cor")
                                                           ),
                                                  tabPanel(" Contributions Plot",
                                                           plotOutput("plot_con")
                                                           ),
                                                  tabPanel("Individual Plot",
                                                           plotOutput("plot_ind")
                                                           ),
                                                  tabPanel("Biplot",
                                                           plotOutput("plot_bi")
                                                           ),
                                                  tabPanel("Clustering",
                                                           plotOutput("plot_cluster")
                                                           )
                                                 ),
                                       )
                                       
                                   )
                                   
                                   
                                   ),
                          
                          
                          
                          
                          "----",
                          "Learn More",
                          tabPanel("More information",
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
                                       understand Canada’s population, resources, economy, society, and culture.
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
                                       “How accessible are Vancouver’s cultural amenities (libraries, museums, 
                                       art galleries, and theatres) using the current transit system?”",
                                       
                                       tags$br(),tags$br(),tags$h4("Methodology/Results:"),
                                       "To address the computational intensity of multimodal shortest path routing, we use Conveyal’s R5 
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
                                       
                                   )
                                   
                                   
                                   
                          ),
                          tabPanel("Data Explorer",
                                   tabPanel("summary_statistics", DT::dataTableOutput("summary_table")),
                                   # Create a new Row in the UI for selectInputs
                                   fluidRow(
                                       column(4,
                                              selectInput("weights",
                                                          "Weights:",
                                                          choices=c("Yes"="yes","No"="no"),
                                                          selected = "yes")
                                       ),
                                       column(4,
                                              selectInput(inputId="nearest",
                                                          label="N Amennity",
                                                          choices=c("All Amenity"="avg_time_to_any_amenity",
                                                              "Nearest Amenity"="time_to_nearest_amenity"),
                                                          selected = "All Amenity")
                                       ),
                                       tags$br(), 
                                       column(4,h4("Click the row to select data !"))
                                      
                                     
                                       ),
                                 
                                       
                                   plotOutput("plot_1", click = "plot_click")
                                   #plotOutput("plot_2",click = "plot_click"),
                       
                                   )
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
        amn_name <- input$type_kep
        html_file <-  glue('{amn_name} Score Kepler')
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
    
    getPage_keptime <- reactive({ 
        amn_name <- input$type_kep
        day <- input$day_kep
        html_file <-  glue('{amn_name} time {day}')
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
    
    # dynamic file calling kepler time window map
    output$keplertime <- renderUI({
        tags$iframe(seamless="seamless", src=paste0('keptime', getPage_keptime()),
                    style="position: absolute; top: 0; right: 0; bottom: 0: left: 0;",
                    width='100%',
                    height='100%') # dynamic height (100%) doesn't work so I set it manually
    })
    
    ##PCA analysis
    #updates df 
    
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
    
    
    
    
    # select the data table 
    output$summary_table = DT::renderDataTable({
        sumstat_df
    })
    

    
   ## add alet
    observeEvent(input$button, {
      # Show a modal when the button is pressed
      shinyalert("Dont worry!", "Select row from data table!", type = "info")
    })
    # plot based on the selected row shows that  total dissemination blocks
    output$plot_1 <- renderPlot({
        s = input$summary_table_rows_selected
        
        sumstat_df <- sumstat_df[s,]
        
        cities_to_keep<-sumstat_df$subdiv
        # filter rows on these cities
        all_data_small <- all_df %>% filter(subdiv %in% cities_to_keep)# & weight == 'no')
        
        # total dissemination blocks of the city of selection'
        # p1<-all_data_small %>% 
        #     group_by(subdiv) %>%
        #     summarize(freq = n()) %>%
        #     arrange(desc(freq)) %>%
        #     ggplot(aes(x = freq, y = reorder(subdiv, +freq))) +
        #     geom_bar(stat='identity') +
        #     theme_classic() +
        #     labs(x = 'Count', y = '') +
        #     geom_text(aes(label=freq), size = 2,  vjust = 0.1, hjust=-0.25)
        

 
        
        ###
       
        legend_ord <- levels(with(all_data_small, reorder(factor(subdiv), -avg_score_to_nearest_amenity, na.rm = TRUE)))
        
        p2<-all_data_small %>% filter(weight == input$weights) %>%
            ggplot(aes(y =reorder(factor(subdiv), avg_score_to_nearest_amenity, na.rm = TRUE),
                       x = avg_score_to_nearest_amenity)) +
            geom_violin(aes(fill = subdiv), scale = 'width', alpha = 0.4, draw_quantiles = c(0.5), size = 0.5) + 
            scale_fill_discrete(breaks=legend_ord) +
            scale_x_continuous("Average Accessibility Score",limits = c(0, 0.3), breaks=c(0,0.1,0.2,0.3)) +
            guides(fill=guide_legend(title= 'Subdivision')) +
            theme_minimal() +
            theme(aspect.ratio = 1,
                  text = element_text(size=20),
                  panel.grid.major.x = element_line(colour="lightgray", size=0.05),
                  panel.grid.major.y = element_line(colour="lightgray", size=0.05),
                  panel.grid.minor.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) 
        
        
        ##
        
        all_data_small%>%select(subdiv,input$nearest)->sub
        # 
        #all_data_small%>%select(subdiv,"time_to_nearest_amenity")->sub
        names(sub)[names(sub) == input$nearest]<-"amn"


        
        

        legend_ord <- levels(with(sub, reorder(factor(subdiv), amn, na.rm = TRUE)))
        p3<-sub %>%
            ggplot(aes(y =reorder(factor(subdiv),amn, na.rm = TRUE), x = (amn)))+
            geom_violin(aes(fill = subdiv), scale = 'width', alpha = 0.4, draw_quantiles = c(0.5), size = 0.5) +
            scale_fill_discrete(breaks=legend_ord) +
            scale_x_continuous("Average Time in Minutes") +
            guides(fill=guide_legend(title= 'Subdivision')) +
            theme_minimal() +
            theme(aspect.ratio = 1,
                  text = element_text(size=20),
                  panel.grid.major.x = element_line(colour="lightgray", size=0.05),
                  panel.grid.major.y = element_line(colour="lightgray", size=0.05),
                  panel.grid.minor.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())

        ggarrange(p2,p3)
    })
    

    

}
shinyApp(ui, server)
