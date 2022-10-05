library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)

here::here()
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Property Data"),
  # Sidebar
  dashboardSidebar(disable = TRUE),
  # Body
  dashboardBody(
    includeCSS(here::here("Week3", "Week3_myOwn", "www","style.css")),
    chooseSliderSkin(skin = "Shiny", color = "purple"),
    fluidRow(
      box(
        width = 6, height = 375, title = "Inputs", status = "primary",
        uiOutput("propertyType",
                 height=100,
                 width="90%"),
        uiOutput("propertyPrice",
                 height=100,
                 width="90%"),
        uiOutput("bed_count",
                 height=100,
                 width="90%")),
      tabBox(
        width = 6, height = 375, title = "Plots",
          tabPanel("Scatter Plot",
                   echarts4rOutput("property_scatter",
                                   height = 300)),
          tabPanel("Bar Plot",
                   echarts4rOutput("property_bar",
                                   height = 300)))),
    hr(),
    fluidRow(
      tabBox(
        width = 6, height = "100%",
        tabPanel("Cute Cat",
                imageOutput("myImage",
                            height = 300)),
        tabPanel("Ugly Table",
                 DTOutput("propertyDT",
                          height = 300))),
      box(
        width = 6, height = "100%", title = "Property Plot",status = "primary",
        leafletOutput("property_map", height = 300)
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
# load Data
  # gs4_auth(email = "jspeh456@gmail.com") # opens authentification Browser , once verified, enter a-mail
  # propertyTable <- range_read(ss = "https://docs.google.com/spreadsheets/d/1u_2Nn_ZeN0VdEGYJewQ2bJd_1KZzzjjUpQLwZ6_r8Uw/edit#gid=0")
  # propertyTable$property_area <- unlist(propertyTable$property_area)
  
  propertyTable = read_csv(here::here("Week3", "Week3_myOwn", "property_data.csv"),
                            show_col_types = FALSE)
  # inputs
  output$propertyType = renderUI({
    choices = unique(propertyTable$property_type)
    selectInput("property_type", "Select Property Type", choices = choices,
                selected = choices,
                multiple = TRUE)})
  
  output$propertyPrice = renderUI({
    min = min(propertyTable$property_price)
    max = max(propertyTable$property_price)
    sliderInput("max_price", "Select Max. price", value = max, min = min, max = max)})
  
  output$bed_count = renderUI({
    min = min(propertyTable$count_bed)
    max = max(propertyTable$count_bed)
    numericInput("bed_count", "Select number of beds",
                 value = max, min = min, max = max)})
  
  # Data wrangling
reactive_property_data = reactive({
  req(input$property_type, input$max_price, input$bed_count)
  property_table|>
    filter(property_type %in% input$property_type,
           property_price <= input$max_price,
           count_bed >= input$bed_count)})  
    
  output$myImage = renderImage({ 
    filename <- normalizePath(file.path(here::here("Week3","Week3_myOwn","cat.jpg")))
    list(src = filename,
         alt = "Here is an image of a cute cat!")}, 
    deleteFile = FALSE)
  
  # Plot location of units
  output$property_map <- renderLeaflet({
    reactive_property_data()%>%
      mutate(popup=paste("<center> <b>Address </b> <br>", address, "</center>"))%>%
      leaflet()%>%
      addTiles()%>%
      addMarkers(lng = ~long,
                 lat = ~lat,
                 label = ~property_id,
                 popup = ~popup)
      })
  # Create Datatable
  output$propertyDT <- renderDT({
    datatable(reactive_property_data(),
              options = list(scrollY = "25vh", scrollX = "150%",
                             autoWidth = TRUE, 
                             columnDefs = list(list(width = '300px', targets = c(9)))))%>%
                formatStyle(0,lineHeight='50%')

    
  })
  # Plot data
  output$property_scatter = renderEcharts4r({
    reactive_property_data()%>%
      group_by(property_type)%>%
      e_charts(property_area)%>%
      e_scatter(property_price, name = "Property Price", symbol_size = 15)%>%
      e_tooltip()%>%
      e_axis_labels(y = "Price", x = "ID")
#browser()
  })
  
  output$property_bar = renderEcharts4r({
    reactive_property_data()%>%
      #group_by(property_type)%>%
      e_charts(property_id)%>%
      e_bar(count_bed, symbol_size = 15)%>%
      e_tooltip()%>%
      e_tooltip()
  })  
  
  
  }

# Run the application
shinyApp(ui = ui, server = server)
