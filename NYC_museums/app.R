#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)

setwd("~/NYCmuseums/NYC_museums")
zipmodzcta <- readRDS("zipmodzcta.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Number of NYC Museums by Zip Code"),

    # Sidebar with a slider input for number o f bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "zip",
                        label = "Select a zip code:",
                        choices = unique(zipmodzcta$MODZTCA))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectzip <- reactive({
    z <- zipmodzcta |>
      filter(MODZCTA == input$zip) 
    return(z)
    })
  
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(palette = "RdPu", 6, domain = zipmodzcta$n)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g museums",
      selectzip()$MODZCTA, selectzip()$n) |>
      lapply(htmltools::HTML)
    
    selectzip() |>
      st_transform(crs = "+init=epsg:4326") |>
      leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(-73.9, 40.7, zoom = 10) |>
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(selectzip()$n),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) |>
      addLegend("bottomright",
                pal = pal,
                values = ~ n,
                bins = c(1,2,3,4,5,6),
                title = "Number of Museums",
                opacity = 0.7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
