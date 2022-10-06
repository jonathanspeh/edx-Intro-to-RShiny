library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(googlesheets4)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Property Data"),
  # Sidebar
  dashboardSidebar(disable = TRUE),
  # Body
  dashboardBody(
        box(width = 12,
          DTOutput("propertyDT",
                   height = 300))))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # options(
  #   gargle_oauth_email = TRUE,
  #   gargle_oauth_cache = "testDeploy/.secrets")
  # googlesheets4::gs4_auth()
  propertyTable = read.csv("data/property_data.csv")
  
  # propertyTable <- range_read(ss = "https://docs.google.com/spreadsheets/d/1u_2Nn_ZeN0VdEGYJewQ2bJd_1KZzzjjUpQLwZ6_r8Uw/edit#gid=0")
  # propertyTable$property_area <- unlist(propertyTable$property_area)
  
  
  # 
  # propertyTable <- read_csv(here::here("Week3", "Week3_myOwn", "www", "property_data.csv"),
  #  show_col_types = FALSE)
  # 
  # load(file = here::here("Week3", "Week3_myOwn", "propDat.RData"))
  # propertyTable = mpd
  
  # Create Datatable
  output$propertyDT <- renderDT({
    datatable(propertyTable)})
}

# Run the application
shinyApp(ui = ui, server = server)


