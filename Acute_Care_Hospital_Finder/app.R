#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Uncomment and run to install the following packages if running this code for the first time
# IMPORTANT: Comment out the install functions before publishing the Shiny app to shinyapp.io
# install.packages("tidyverse", "shiny", "htmltools", "leaflet")
library(shiny)
library(tidyverse)
library(htmltools)
library(leaflet)

locations <- read_csv("data/locations.csv")
hospital_data <- read_csv("data/hospitals.csv") 
measures <- read_csv("data/hvbp_tps.csv")

measures <- measures %>%
  select(c(2, 9, 11, 13, 15)) %>%
  rename(clinical_outcomes = `Unweighted Normalized Clinical Outcomes Domain Score`) %>%
  rename(safety = `Unweighted Normalized Safety Domain Score`) %>%
  rename(person_and_community = `Unweighted Person and Community Engagement Domain Score`) %>%
  rename(efficiency_and_cost = `Unweighted Normalized Efficiency and Cost Reduction Domain Score`)

hospital_data <- full_join(hospital_data, measures, by = "Facility ID")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Acute Care Hospital Finder"),

    sidebarLayout(
        sidebarPanel(
            selectInput("state", label = "Select State:",
                        choices = locations$State),
            selectInput("county", label = "Select County:",
                        choices = NULL)
        ),
  
        mainPanel(
          tabsetPanel(
            tabPanel("Hospital Locations",
                     fluidRow(htmltools::HTML("<br>"), h4("The Best Hospital in your County by Star Rating")),
                     leafletOutput("MapPlot", height = "75vh", width = "60vw")),
            tabPanel("County Statistics",
                     plotOutput("statistics", height = "75vh", width = "60vw"))
          )
        )
    )
)
?htmltools::HTML

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$state,{
    temp_df <- locations %>%
      filter(State == input$state) %>%
      arrange(County)
    
    updateSelectInput(session, 'county',
                      choices = unique(temp_df$County))
  })
  
  output$statistics <- renderPlot({
    hospital_data %>%
      filter(State == input$state) %>%
      filter(County == input$county) %>%
      mutate(clinical_outcomes = as.numeric(clinical_outcomes), person_and_community = as.numeric(person_and_community), safety = as.numeric(safety), efficiency_and_cost = as.numeric(efficiency_and_cost)) %>%
      group_by(County) %>%
      summarise(`Clinical Outcomes` = mean(clinical_outcomes), `Person & Community Engagement` = mean(person_and_community), Safety = mean(safety), `Efficiency and Cost Reduction` = mean(efficiency_and_cost))  %>%
      pivot_longer(cols = 2:5, names_to = "Quality Measures", values_to = "Performance") %>%
      ggplot(text = element_text(size = 20)) +
      geom_bar(mapping = aes(x = reorder(x = `Quality Measures`, Performance), y = Performance, fill = `Quality Measures`), stat = "identity") +
      scale_fill_brewer(palette = "greens") +
      labs(title = "Key Hospital Domains Scores", subtitle = "Higher score indicates better performance") +
      xlab("Quality Measures") +
      ylab("Score") +
      theme(text = element_text(size = 18)) +
      coord_flip()
  })
    
  MapPlot <- reactiveValues()

  output$MapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    temp_df <- locations %>%
      filter(State == input$state) %>%
      filter(County == input$county)  %>%
      head(1)
    
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
      setView(lng = as.numeric(MapPlot$long), lat = as.numeric(MapPlot$lat), zoom = 20) %>%
      addMarkers(lng = as.numeric(MapPlot$long), lat = as.numeric(MapPlot$lat),
                 label = hosplabels, popup = hosplabels)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
