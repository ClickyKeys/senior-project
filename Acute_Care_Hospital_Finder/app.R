#
# This is a Shiny web application which shows one of the best hospitals in the selected county and county-level hospital quality domain scores (where available).
# You can run the application by clicking the 'Run App' button above.
#


# Uncomment and select/run the 'install.packages' lines to install/update the following packages if running this code for the first time
# # IMPORTANT: Comment out the install functions before running the app or publishing the Shiny app to shinyapp.io
# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("htmltools") 
# install.packages("leaflet")

library(htmltools)
library(shiny)
library(tidyverse)
library(leaflet)

# Read in the hospital locations data, general hospital information, and quality domain scores
# WARNING: Using new data apart from what was downloaded from GitHub may require significant changes to the code
locations <- read_csv("data/locations.csv")
hospital_data <- read_csv("data/hospitals.csv") 
measures <- read_csv("data/hvbp_tps.csv")

# Select the quality measure domain scores to display in the app
measures <- measures %>%
  select(c(2, 9, 11, 13, 15)) %>%
  rename(clinical_outcomes = `Unweighted Normalized Clinical Outcomes Domain Score`) %>%
  rename(safety = `Unweighted Normalized Safety Domain Score`) %>%
  rename(person_and_community = `Unweighted Person and Community Engagement Domain Score`) %>%
  rename(efficiency_and_cost = `Unweighted Normalized Efficiency and Cost Reduction Domain Score`)

# Join the hospital and quality measure data into a single dataframe
hospital_data <- full_join(hospital_data, measures, by = "Facility ID")

# Define UI for application that generates two tabs
# First tab - Generates a leaflet map showing the location of a top rated hospital
# Second tab - Generates a bar chart showing county-level performance in key quality domains
ui <- fluidPage(

    # Application title
    titlePanel("Acute Care Hospital Finder"),

    # Drop-downs for selecting options
    sidebarLayout(
        sidebarPanel(
            selectInput("state", label = "Select State:",
                        choices = locations$State),
            selectInput("county", label = "Select County:",
                        choices = NULL)
        ),
  
        # The main display area, tab panel containing two tabs
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

# Define server logic
server <- function(input, output, session) {

  # Depending on the state selected, generate a list of counties in that state with Hospital data
  observeEvent(input$state,{
    temp_df <- locations %>%
      filter(State == input$state) %>%
      arrange(County)
    
    updateSelectInput(session, 'county',
                      choices = unique(temp_df$County))
  })
  
  # Depending on the county selection made, generate a bar chart of average hospital quality domain performance scores
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
    
  # Create an object for storing reactive values
  MapPlot <- reactiveValues()

  # Render a leaflet map
  output$MapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # For the selected state and county, store reactive values for the hospital name, address, star rating, lat, and long
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
    
    # Create a label for the hospital plotted on the map, which contains the reactive values stored above
    hosplabels <- sprintf("%s<br>%s, %s<br>Star Rating: %s",
                          MapPlot$name, MapPlot$address, MapPlot$city, MapPlot$rating) %>%
      lapply(htmltools::HTML)
    
    # Set the markers on the generated leaflet map using the labels created above
    # Set the view of the map to the lat & long of the hospital being plotted
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
