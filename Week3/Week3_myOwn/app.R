library(shiny)
library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Week 3 - Simple App"),
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(),
        dashboardBody(
           plotOutput("propertyPlot")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  gs4_auth() # opens authentification Browser 
  propertyTable=range_read(ss="https://docs.google.com/spreadsheets/d/1u_2Nn_ZeN0VdEGYJewQ2bJd_1KZzzjjUpQLwZ6_r8Uw/edit#gid=0")
  propertyTable$property_area=unlist(propertyTable$property_area)

    output$propertyPlot <- renderPlot({
      options(scipen = 10)
      propertyTable%>%filter(area_unit!="acres")%>%
        ggplot(aes(y=property_area, x=property_price, size = factor(count_bed)))+
        geom_point(color="green")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
