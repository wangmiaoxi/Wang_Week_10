#For this assignment you will make a Shiny app using American Community Survey data.
#Please write a Shiny app with the following features:
  #* a choice of which state to plot
  #* a choice of plotting either median household income (B19013_001), median gross rent (B25064_001), or the ratio of median gross rent to median household income
  #* a plot of the chosen variable in the chosen state.
library(tidyverse)
library(shiny)
library(tidycensus)

source("api-keys.R")
census_api_key(api.key.census)

ui <- fluidPage(
  titlePanel("American Community Survey"),
  
  sidebarLayout(
    
    # Select a State
    sidebarPanel(
      #input
      selectInput("State", "State", choices = state.abb, selected = "IN"),
      
      radioButtons("Type", "Type", choices = list("median_gross_rent", "median_household_income", "ratio"), 
                   selected = "ratio")
      ),
    
    #result
    mainPanel(plotOutput("Plot"))
  )
)


server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(geography = "tract",
            variables = c(median_household_income = "B19013_001", median_gross_rent = "B25064_001"),
            state = "IN",
            geometry = TRUE
            ) %>%
      .[, -5] %>%
      data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  output$Plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type) + 
      scale_fill_gradientn(colours = rainbow(10))
  })
  
}
 
shinyApp(ui = ui, server = server)
