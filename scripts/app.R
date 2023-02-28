####################
# Load libraries 
library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(corrplot)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(DescTools)
library(ggmap)
library(lubridate)
library(cluster)
library(leaflet)

####################
# CLEAN ENVIRONMENT 
rm(list = ls())
set.seed(2022)

#########################################################################################
# Initial Setup

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
location <- "../input/data_SQRFT.csv"

#########################################################################################

# read the data

data <- read_csv(location)
data <- data[, 2:ncol(data)]
# numeric_cols <- c("sale_price", "gross_square_feet", "land_square_feet", "residential_units", "commercial_units") #-----?
numeric_cols <- c("sale_price", "gross_square_feet", "land_square_feet", "residential_units")
factor_cols <- c("neighborhood", "tax_class_at_present", "building_class_category") #------?
#########################################################################################

# To obtain New York Map, Use Geocode Google AP

# Register the google api key
register_google(key = "XYZ")

# Set center to New York
loc = c(lon = -73.97, lat = 40.79)
newyork.map <- get_map(location = loc, zoom = 13, maptype = "roadmap", source = "google", color = "bw", bbox = bbox)

#########################################################################################


# summary of the data
# glimpse(data)

# find summary of the data
# summary(data)

####################
# Data Visualization
####################

####################
# Create UI
####################

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("cerulean"),
  
  # App title 
  titlePanel("Manhattan Real Estate Tool"),
  
  # create tabs
  tabsetPanel(
    
    # TAB 0 - LOGS
    # tabPanel(
    #   "Logs",
    #   # Clear Logs on click
    #   verbatimTextOutput("logs")
    # ),
    
    #########################################################################################
    # TAB 1 - INFO
    
    
    #########################################################################################
    # TAB 2- DATA TABLE
    tabPanel(
      "Data Selection",
      sidebarLayout(
        
        # for input
        sidebarPanel(
          
          # display text
          h3("Filter Features"),
          
          # select neighborhood
          selectInput("neighborhoodTAB2", "Neighborhood", choices =sort(unique(data$neighborhood)), multiple = TRUE, selected = sort(unique(data$neighborhood))),
          
          # select tax class at present 
          checkboxGroupInput("taxClassTAB2", "Tax Class", choices = unique(data$tax_class_at_time_of_sale), selected = unique(data$tax_class_at_time_of_sale)),
          
          # select sale price
          sliderInput("salePriceTAB2", "Sale Price in $", min = min(data$sale_price), max = max(data$sale_price), value = c(min(data$sale_price), max(data$sale_price)), step = 100),
          
          # select land square feet
          sliderInput("landSquareFeetTAB2", "Land Square Feet", min = min(data$land_square_feet), max = max(data$land_square_feet), value = c(min(data$land_square_feet), max(data$land_square_feet)), step = 10),
          
          # select residential units
          sliderInput("residentialUnitsTAB2", "Residential Units", min = 1, max = max(data$residential_units), value = c(1, max(data$residential_units)), step = 1), # very important , min = 1!!!! (residential units can't be 0)
          # Select Building Class Category at Present
          selectInput("buildingClassCategoryTAB2", "Building Class Category", choices = sort(unique(data$building_class_category)), multiple = TRUE, selected = sort(unique(data$building_class_category)))
          
        ),
        
        # for output
        mainPanel(
          tabsetPanel(
            tabPanel("Table",
                     
                     # Display features to select multiple features using drop down menu, start with blank
                     selectInput("featuresTAB2", "Select Columns to Display:", colnames(data), multiple = TRUE),
                     
                     DT::dataTableOutput("data_summaryTAB2")),
            tabPanel("Correlation Plot",plotOutput("data_corrTAB2"))
          )
        )
      )
    ), #end of TAB2
    
    ########################################################################################
    # TAB 3 - DATA VISUALIZATION
    
    tabPanel(
      "Data Visualization",
      tabsetPanel(
        
        # categorical tab within  TAB3
        tabPanel("Categorical Features", 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     # display
                      h3("Select Category to Display against a Quantity"),
                     
                     # select a numerical variable to plot
                     selectInput("viz_featureTAB3_cat_y", "Quantity", choices = numeric_cols, multiple = FALSE, selected = "sale_price"),
                     
                     # select variable to plot against sale price (Categorical)
                     selectInput("viz_featureTAB3_cat_x", "Category", choices = factor_cols, multiple = FALSE, selected = "neighborhood"),
                     
                     # checkbox for whether outlier should be included in the boxplot
                     checkboxInput("outliersTAB3", "Exclude outliers", FALSE),
                     
                     # radio button to select type of plot
                     radioButtons("plotTypeTAB3", "Select Plot Type", c("Boxplot", "Violin Plot"))
                     
                   ),
                   mainPanel(
                     plotOutput("viz_plotTAB3_cat")
                   )
                 )
                 
        ),#end of categorical tab within TAB3,
        
        # numerical tab within TAB3
        tabPanel("Numerical Features",
                 sidebarLayout(
                   
                   sidebarPanel(

                    # display
                    h3("Select 2 Quantities to Display against each other"),

                    # select a numerical variable to plot
                    selectInput("viz_featureTAB3_num_y", "Quantity y", choices = numeric_cols, multiple = FALSE, selected = "sale_price"),
                     
                    # select a numerical variable to plot
                    selectInput("viz_featureTAB3_num_x", "Quantity x", choices = numeric_cols, multiple = FALSE, selected = "sale_price")
                     
                   ),
                   mainPanel(
                     plotOutput("viz_plotTAB3_num")
                   )
                 )
        )#end of numerical tab within TAB3
        
      )
    ),#end of TAB3
    
    #########################################################################################
    # TAB 4 - Clustering
    
    tabPanel(
      "Data Modelling",
      sidebarLayout(
        sidebarPanel(

          h3("Select Features to Cluster"),

          selectInput("cluster_featureTAB4", "Features", choices = numeric_cols, multiple = TRUE),
          #sliderInput("cluster_valuesTAB4", "Select Column Values", min = min(data$land_square_feet), max = max(data$land_square_feet), value = c(min(data$land_square_feet), max(data$land_square_feet)), step = 10),
          
          h4("For K-Means Clustering, pick k value at the elbow point of the graph shown."),
          sliderInput("cluster_kTAB4", "Number of Clusters", min = 2, max = 10, value = 5, step = 1),

          # Multiple input for Applying log transformation
          selectInput("logTAB4", "Apply Log Transformation on Selected Features", choices = numeric_cols, multiple = TRUE),

          h3("Elbow Plot"),
          plotOutput("elbow_plotTAB4"),
          h5("Within Sum of Squares (WSS) for optimal k:"), textOutput("result2TAB4")

        ),
        mainPanel(

            h3('Leaflet Cluster Map')
            ,leafletOutput("leaflet_cluster_mapTAB4"),

            h3("Map"),
            plotOutput("cluster_plotTAB4")



        )
      )
    )
  )
)

server <- function(input, output, session) {
  

  
  #########################################################################################
  # TAB 1 - INFO
  
  #########################################################################################
  # TAB 2 - DATA TABLE
  
  table <- reactive({
    
    # filter data based on user input
    data %>%
      # Filters
      filter(
        data$sale_price >= input$salePriceTAB2[1] & data$sale_price <= input$salePriceTAB2[2] &
          data$land_square_feet >= input$landSquareFeetTAB2[1] & data$land_square_feet <= input$landSquareFeetTAB2[2] &
          data$tax_class_at_present %in% input$taxClassTAB2 &
          data$building_class_category %in% input$buildingClassCategoryTAB2 &
          data$neighborhood %in% input$neighborhoodTAB2 &
          data$residential_units >= input$residentialUnitsTAB2[1] & data$residential_units <= input$residentialUnitsTAB2[2] 
      )
  })
  
  output$data_summaryTAB2 <- DT::renderDataTable({
    table()%>%
      select(input$featuresTAB2)
    
  })
  
  output$data_corrTAB2 <- renderPlot({
    
    # filter data based on user input
    table() %>%
      select(numeric_cols)%>%
      cor() %>%
      corrplot::corrplot.mixed(order = 'hclust')
    
    
  }, height = 850, width = 850)
  
  #########################################################################################
  # TAB 3 - DATA VISUALIZATION
  
  # CATERGORICAL TAB WITHIN TAB3
  
  output$viz_plotTAB3_cat <- renderPlot({
    
    req(input$viz_featureTAB3_cat_y)
    req(input$viz_featureTAB3_cat_x)
    
    if(input$outliersTAB3) {
      outlier.shape = NA
    } else {
      outlier.shape = 19
    }
    
    if(input$plotTypeTAB3 == "Boxplot") {
      plot_type <- geom_boxplot(outlier.shape = outlier.shape)
    } else {
      plot_type <- geom_violin()
    }
    
    # filter data based on user input
    table() %>%
      ggplot(aes_string(x = input$viz_featureTAB3_cat_x, y = input$viz_featureTAB3_cat_y)) +
      plot_type +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = input$viz_featureTAB3_cat_x, y = input$viz_featureTAB3_cat_y)
  })
  
  # NUMERICAL TAB WITHIN TAB3
  
  output$viz_plotTAB3_num <- renderPlot({
    
    req(input$viz_featureTAB3_num_y)
    req(input$viz_featureTAB3_num_x)
    
    # filter data based on user input
    table() %>%
      ggplot(aes_string(x = input$viz_featureTAB3_num_x, y = input$viz_featureTAB3_num_y)) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = input$viz_featureTAB3_num_x, y =  input$viz_featureTAB3_num_y)  
    
  })
  
  #########################################################################################
  # TAB 4 - CLUSTERING

  # create a reactive expression to store table
  original_data <- reactive({table()})

  # create a reactive expression to extract features and scale them
    residential_data <- reactive({

        req(input$cluster_featureTAB4)

        # Log transformation
        if(length(input$logTAB4) > 0) {

            preprocess <- table() %>%
            mutate_at(input$logTAB4, as.numeric) %>%
            mutate_at(input$logTAB4, log)
        } else {
            preprocess <- table()
        }

        # feature extraction
        preprocess %>%
            mutate(age = year(sale_date) - year_built) %>%
            mutate(gross_square_feet_per_unit = gross_square_feet / total_units) %>%
            select(c(input$cluster_featureTAB4, "age", "gross_square_feet_per_unit")) %>%
            RobScale()
    })


  plot_elbow <- reactive({
    # select k for k-means clustering using elbow method
    wssplot <- function(data, nc=15, seed=seed){
      wss <- (nrow(data)-1)*sum(apply(data,2,var))
      for (i in 2:nc){
          wss[i] <- sum(kmeans(data, centers=i, nstart=20)$withinss)
      }
      plot(1:nc, wss, type="b", xlab="Number of Clusters (k)",
            ylab="Within Sum of Squares (WSS)")
    }
    wssplot(residential_data(), nc = 15)
  })

  # render plot_elbow
  output$elbow_plotTAB4 <- renderPlot({
    plot_elbow()
  })

  # perform kmeans clustering on scaled data using user input and return cluster centers 
  kmeans_vals <- reactive({
    req(input$cluster_featureTAB4)
    req(input$cluster_kTAB4)

    kmeans(residential_data(), centers = input$cluster_kTAB4, nstart = 20)
  })

#   Calculate the silhouette width for the selected k
    silhouette_width <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)
    
        # Calculate the silhouette width for the selected k
        silhouette(kmeans_vals()$cluster, dist(residential_data()))
    })



  # add cluster labels to original data
  clustered_data <- reactive({
    original_data() %>%
      mutate(kmeans = as.factor(kmeans_vals()$cluster))
  })

  # visualize clusters on New York map
  output$cluster_plotTAB4 <- renderPlot({
    # Plot k-means values on the map using Latitude and Longitude
    ggmap(newyork.map) +
        geom_point(
            data=clustered_data(),
            aes(x=longitude,y=latitude,color=kmeans)
      )

  }, width = 550, height = 550)

    # Calculate WSS
    wss_val <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)


        # Calculate WSS for k = input$cluster_kTAB4
        wss <- (nrow(residential_data())-1)*sum(apply(residential_data(),2,var))
        for (i in 2:input$cluster_kTAB4){
            wss[i] <- sum(kmeans(residential_data(), centers=i, nstart=20)$withinss)
        }
        wss[input$cluster_kTAB4]
    })

    r1 <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)

        paste("Silhouette width" = silhouette_width())
    })
    r2 <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)

        paste("Within groups sum of squares" = wss_val())
    })
    r3 <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)

        paste("Cluster centers" = kmeans_vals()$centers)
    })
    r4 <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)

        paste("Cluster between groups sum of squares" = kmeans_vals()$betweenss)
    })
    r5 <- reactive({
        req(input$cluster_featureTAB4)
        req(input$cluster_kTAB4)

        paste("Cluster total sum of squares" = kmeans_vals()$tot.withinss)
    })




    output$result1TAB4 <- renderText({ r1() })
    output$result2TAB4 <- renderText({ r2() })
    output$result3TAB4 <- renderText({ r3() })
    output$result4TAB4 <- renderText({ r4() })
    output$result5TAB4 <- renderText({ r5() })

    output$leaflet_cluster_mapTAB4 <- renderLeaflet({
        # Plot values on the map using Latitude and Longitude using leaflet, mark clusters using the kmeans column
        leaflet(newyork.map) %>%
            addTiles() %>%
            addMarkers(data = clustered_data(), lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(), popup = ~paste("Latitude", latitude, ", ","Longitude", longitude, ", ","Sale price", sale_price, ", ","Sale date", sale_date, ", ","Building class category", building_class_category, ", ","Tax class at present", tax_class_at_present, ", ","Tax class at time of sale", tax_class_at_time_of_sale, ", ","Building class at present", building_class_at_present, ", ","Residential units", residential_units, ", ","Commercial units", commercial_units, ", ","Total units", total_units, ", ","Land square feet", land_square_feet, ", ","Gross square feet", gross_square_feet))
            
        })

    # TAB 0 - LOGS
    # output$logs <- renderText({
    #     paste("\nLogs:", results_to_display())
    # })
  

}

####################
# Run the app
####################
shinyApp(ui, server)
