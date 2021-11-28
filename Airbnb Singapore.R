---
output: html_document
runtime: shiny
---
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)
library(tidyverse)
library(leaflet)
library(highcharter)

df <- read_csv('listings.csv')
df <- df %>% mutate(avg_price = price/minimum_nights, min_cost = price*minimum_nights)
private <- df %>% filter(room_type == 'Private room')
shared <- df %>% filter(room_type == 'Shared room')
apt <- df %>% filter(room_type == 'Entire home/apt')

pmap <- leaflet(private %>% select(longitude, neighbourhood_group, neighbourhood, latitude, price)) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 10) %>%
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
smap <- leaflet(shared %>% select(longitude, neighbourhood_group, neighbourhood, latitude, price)) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 10) %>%
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
amap <- leaflet(apt %>% select(longitude, neighbourhood_group, neighbourhood, latitude, price)) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 10) %>%
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

pbox <- ggplotly(private %>% filter(!(abs(avg_price - median(avg_price)) > 2*sd(avg_price))) %>%
                   ggplot(aes(neighbourhood_group, avg_price, fill = neighbourhood_group)) + 
                   geom_boxplot() + labs(title = "Based on neighbourhood group", x = "neighbourhood", y = "average price per day ($)") + 
                   theme_classic() + theme(legend.position = "none"))
sbox <- ggplotly(shared %>% filter(!(abs(avg_price - median(avg_price)) > 2*sd(avg_price))) %>%
                   ggplot(aes(neighbourhood_group, avg_price, fill = neighbourhood_group)) + 
                   geom_boxplot() + labs(title = "Based on neighbourhood group", x = "neighbourhood", y = "average price per day ($)") + 
                   theme_classic() + theme(legend.position = "none"))
abox <- ggplotly(apt %>% filter(!(abs(avg_price - median(avg_price)) > 2*sd(avg_price))) %>%
                   ggplot(aes(neighbourhood_group, avg_price, fill = neighbourhood_group)) + 
                   geom_boxplot() + labs(title = "Based on neighbourhood group", x = "neighbourhood", y = "average price per day ($)") + 
                   theme_classic() + theme(legend.position = "none"))

ui <- dashboardPage(
  md = TRUE,
  skin = "red",
  dashboardHeader(
    title = "Airbnb Singapore",
    titleWidth = 200
  ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Private Rooms", tabName = "private", icon = icon("user")),
      menuItem("Shared Rooms", tabName = "shared", icon = icon("users")),
      menuItem("Entire Apartment", tabName = "apt", icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "private",
        fluidRow(
          valueBox(
            nrow(private),
            "Number of Private Rooms",
            icon = icon("hashtag"),
            color = 'purple',
          ),
          valueBox(
            length(unique(private$neighbourhood)),
            "Number of Locations",
            icon = icon("map-marker-alt"),
            color = 'green'
          ),
          valueBox(
            round(sum(private$price)/nrow(private), 2),
            "Average Price (per night)",
            icon = icon("dollar-sign"),
            color = 'yellow'
          )
        ),
        
        fluidRow(
          box(title = "Locations", status = "primary", solidHeader = TRUE, leafletOutput("pmap")),
          box(title = "Average Price", status = "primary", solidHeader = TRUE, plotlyOutput("pbox"))
        ),
        
        fluidRow(
          box(width = 4, title = "Room Count", status = "info", solidHeader = TRUE, highchartOutput("phchart1")),
          box(width = 4, title = "Room Availability", status = "info", solidHeader = TRUE, highchartOutput("phchart2")),
          box(width = 4, title = "Filter", status = "black", solidHeader = TRUE, selectInput("png", label = h2(strong("Select neighbourhood group")), choices = unique(df$neighbourhood_group)))
        )
      ),
      
      tabItem(
        tabName = "shared",
        fluidRow(
          valueBox(
            nrow(shared),
            "Number of Shared Rooms",
            icon = icon("hashtag"),
            color = 'purple'
          ),
          valueBox(
            length(unique(shared$neighbourhood)),
            "Number of Locations",
            icon = icon("map-marker-alt"),
            color = 'green'
          ),
          valueBox(
            round(sum(shared$price)/nrow(shared), 2),
            "Average Price (per night)",
            icon = icon("dollar-sign"),
            color = 'yellow'
          )
        ),
        
        fluidRow(
          box(title = "Locations", status = "primary", solidHeader = TRUE, leafletOutput("smap")),
          box(title = "Average Price", status = "primary", solidHeader = TRUE, plotlyOutput("sbox"))
        ),
        
        fluidRow(
          box(width = 4, title = "Room Count", status = "info", solidHeader = TRUE, highchartOutput("shchart1")),
          box(width = 4, title = "Room Availability", status = "info", solidHeader = TRUE, highchartOutput("shchart2")),
          box(width = 4, title = "Filter", status = "black", solidHeader = TRUE, selectInput("sng", label = h2(strong("Select neighbourhood group")), choices = unique(df$neighbourhood_group)))
        )
      ),
      
      tabItem(
        tabName = "apt",
        fluidRow(
          valueBox(
            nrow(apt),
            "Number of Apartments",
            icon = icon("hashtag"),
            color = 'purple'
          ),
          valueBox(
            length(unique(apt$neighbourhood)),
            "Number of Locations",
            icon = icon("map-marker-alt"),
            color = 'green'
          ),
          valueBox(
            round(sum(apt$price)/nrow(apt), 2),
            "Average Price (per night)",
            icon = icon("dollar-sign"),
            color = 'yellow'
          )
        ),
        
        fluidRow(
          box(title = "Locations", status = "primary", solidHeader = TRUE, leafletOutput("amap")),
          box(title = "Average Price", status = "primary", solidHeader = TRUE, plotlyOutput("abox"))
        ),
        
        fluidRow(
          box(width = 4, title = "Apartment Count", status = "info", solidHeader = TRUE, highchartOutput("ahchart1")),
          box(width = 4, title = "Apartment Availability", status = "info", solidHeader = TRUE, highchartOutput("ahchart2")),
          box(width = 4, title = "Filter", status = "black", solidHeader = TRUE, selectInput("ang", label = h2(strong("Select neighbourhood group")), choices = unique(df$neighbourhood_group)))
        )
      )
    )
  )
)

server <- function(input, output) {
  output$phchart1 <- renderHighchart(hchart(filter(private, neighbourhood_group == input$png)$min_cost, color = "#B71C1C", name = input$png) %>%
                                       hc_title(text = "Based on Minimum Cost (price * minimum_nights)") %>%
                                       hc_add_theme(hc_theme_ffx()))
  output$shchart1 <- renderHighchart(hchart(filter(private, neighbourhood_group == input$sng)$min_cost, color = "#B71C1C", name = input$sng) %>%
                                       hc_title(text = "Based on Minimum Cost (price * minimum_nights)") %>%
                                       hc_add_theme(hc_theme_ffx()))
  output$ahchart1 <- renderHighchart(hchart(filter(apt, neighbourhood_group == input$ang)$min_cost, color = "#B71C1C", name = input$ang) %>%
                                       hc_title(text = "Based on Minimum Cost (price * minimum_nights)") %>%
                                       hc_add_theme(hc_theme_ffx()))
  output$phchart2 <- renderHighchart(hchart(filter(private, neighbourhood_group == input$png)$availability_365, color = "#336666", name = input$png) %>%
                                       hc_title(text = "Based on number of days in a year") %>%
                                       hc_add_theme(hc_theme_ffx()))
  output$shchart2 <- renderHighchart(hchart(filter(shared, neighbourhood_group == input$sng)$availability_365, color = "#336666", name = input$sng) %>%
                                       hc_title(text = "Based on number of days in a year") %>%
                                       hc_add_theme(hc_theme_ffx()))
  output$ahchart2 <- renderHighchart(hchart(filter(apt, neighbourhood_group == input$ang)$availability_365, color = "#336666", name = input$ang) %>%
                                       hc_title(text = "Based on number of days in a year") %>%
                                       hc_add_theme(hc_theme_ffx()))
  
  output$pmap <- renderLeaflet(pmap)
  output$smap <- renderLeaflet(smap)
  output$amap <- renderLeaflet(amap)
  
  output$pbox <- renderPlotly(pbox)
  output$sbox <- renderPlotly(sbox)
  output$abox <- renderPlotly(abox)
}

shinyApp(ui, server)