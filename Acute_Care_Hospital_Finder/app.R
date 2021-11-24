#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, tidyverse, htmltools, leaflet)

locations <- read_csv("data/locations.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Acute Care Hospital Finder"),

    sidebarLayout(
        sidebarPanel(
            selectInput("state", label = "Select State:",
                        choices = locations$State,
                        selected = locations$State[1]),
            selectInput("county", label = "Select County:",
                        choices = NULL)
        ),

        mainPanel(
           leafletOutput("MapPlot")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$state,{
    temp_df <- locations %>%
      filter(State == input$state)
    
    updateSelectInput(session, 'county',
                      choices = unique(temp_df$County))
  })
  
  MapPlot <- reactiveValues()

  output$MapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    
    temp_df <- locations %>%
      filter(State == input$state) %>%
      filter(County == input$county) 
    
    MapPlot$name <- temp_df %>%
      select(c("Facility Name"))
    MapPlot$address <- temp_df %>%
      select(c("Address"))
    MapPlot$rating <- temp_df %>%
      select(c("star_rating"))
    MapPlot$city <- temp_df %>%
      select(c("City"))
    MapPlot$lat <- temp_df %>%
      select(c("lat"))
    MapPlot$long <- temp_df %>%
      select(c("long"))
    
    hosplabels <- sprintf("%s<br>%s, %s<br>Star Rating: %s",
                          MapPlot$name, MapPlot$address, MapPlot$city, MapPlot$rating) %>%
      lapply(htmltools::HTML)
    
    proxy <- leafletProxy("MapPlot") %>%
      clearMarkers()
    
    proxy %>%
      setView(lng = as.numeric(MapPlot$long), lat = as.numeric(MapPlot$lat), zoom = 15) %>%
      addMarkers(lng = as.numeric(MapPlot$long), lat = as.numeric(MapPlot$lat),
                 label = hosplabels, popup = hosplabels)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
