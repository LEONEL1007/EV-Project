library(tidyverse)
library(shiny)

# Reading CSVs into R
electric_vehicle_population <-read_csv("Electric_Vehicle_Population_Data.csv")

# clean_names from the janitor package, takes the existing column names of your data set, converts them all to lowercase letters and numbers, 
# and separates all words using the underscore character.
electric_vehicle_population <- electric_vehicle_population |> 
  janitor::clean_names()

# for this EDA we are only interested in the following columns; 
# Select all columns except those from base_msrp to x2020_census_tract (inclusive):
electric_vehicle_population <- electric_vehicle_population |> 
  select(!base_msrp:x2020_census_tract)

# Number of EVs registered by model year
ui<- fluidPage(
  
  theme = shinythemes::shinytheme("cerulean"),
  
  titlePanel("EVs Registered in the State of Washington"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "model_years",
        label = "Select Model Year",
        min = 1997,
        max = 2023,
        value = 2011
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tab1", plotOutput(outputId = "plot1")),
        tabPanel("Tab2", plotOutput(outputId = "plot2"))
      )
    )
  )
)


server<-function(input,output, session){
  
  output$plot1 <- renderPlot({
    
    ggplot(filter(electric_vehicle_population, model_year == input$model_years), 
           aes(x = fct_rev(fct_infreq(make)))) + 
      geom_bar() + 
      geom_text(aes(label = after_stat(count)), stat = "count", hjust = -0.2, size = 3.5, colour = "dark green") +
      coord_flip() +
      labs(title = "Electric Vehicle Registrations in Washington State") +
      xlab("Car Make/Manufacturer") +
      ylab("Number of Vehicles Registered") +
      theme_classic()}, res = 96) 
  
  output$plot2 <- renderPlot({
    ggplot(filter(electric_vehicle_population, model_year == input$model_years), 
           aes(x = fct_infreq(electric_vehicle_type))) + 
      geom_bar() +
      geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.25, size = 3.6, colour = "dark green") +
      labs(title = "Battery Electric Vehicles vs. Plug-in Hybrid Electric Vehicles") +
      xlab("Type of EVs") +
      ylab("Number of Vehicles Registered") +
      theme_classic()}, res = 96) 
}

shinyApp(ui = ui, server = server)