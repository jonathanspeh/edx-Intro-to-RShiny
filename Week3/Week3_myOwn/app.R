library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Property Data"),
  # Sidebar
  dashboardSidebar(disable = TRUE),
  # Body
  dashboardBody(
    includeCSS("www/style.css"),
    chooseSliderSkin(skin = "Shiny", color = "purple"),
    fluidRow(
      box(
        width = 6, height = 375, title = "Inputs", status = "primary",
        column(width = 11,
        uiOutput("propertyType",
          height = 100,
          width = "90%"
        ),
        uiOutput("propertyPrice",
          height = 100,
          width = "90%"
        ),
        uiOutput("bed_count",
          height = 100,
          width = "90%"
        )
      )),
      tabBox(
        width = 6, height = 375, title = "Plots",
        tabPanel(
          "Scatter Plot",
          echarts4rOutput("property_scatter",
            height = 300
          )
        ),
        tabPanel(
          "Bar Plot",
          echarts4rOutput("property_bar",
            height = 300
          )
        )
      )
    ),
    hr(),
    fluidRow(
      box(
        width = 6, height = 375, status = "primary",
          DTOutput("propertyDT",
            height = 300)
        ),
      box(
        width = 6, height = 375, title = "Property Plot", status = "primary",
        leafletOutput("property_map", height = 300)
      )
    )
  )  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Google Sheets API authentication flow ----
  # options(gargle_oauth_cache = "Week3/Week3_myOwn/.cache") # designate project-specific cache
  #  gargle::gargle_oauth_cache() # check the value of the option
  #  googlesheets4::gs4_auth()# trigger auth on purpose to store a token in the specified cache
  # cache_directory <- "Week3/Week3_myOwn/.cache/" # can add to config file
   # list.files(cache_directory) # see your token file in the cache
   # googlesheets4::gs4_deauth() # de auth
  
 #  gs4_auth(email = "jspeh456@gmail.com", cache = cache_directory)
  
  # propertyTable <- range_read(ss = "https://docs.google.com/spreadsheets/d/1u_2Nn_ZeN0VdEGYJewQ2bJd_1KZzzjjUpQLwZ6_r8Uw/edit#gid=0")
  propertyTable = read.csv("data/property_data.csv")
  
  
  # inputs
  output$propertyType <- renderUI({
    choices <- unique(propertyTable$property_type)
    selectInput("property_type", "Select Property Type",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })

  output$propertyPrice <- renderUI({
    min <- min(propertyTable$property_price)
    max <- max(propertyTable$property_price)
    sliderInput("max_price", "Select Max. price", value = max, min = min, max = max)
  })

  output$bed_count <- renderUI({
    min <- min(propertyTable$count_bed)
    max <- max(propertyTable$count_bed)
    numericInput("bed_count", "Select number of beds",
      value = min, min = min, max = max
    )
  })

  # Data wrangling
  reactive_property_data <- reactive({
    req(input$property_type, input$max_price, input$bed_count)
    propertyTable |>
      filter(
        property_type %in% input$property_type,
        property_price <= input$max_price,
        count_bed >= input$bed_count
      )
  })


  # Plot location of units
  output$property_map <- renderLeaflet({
    reactive_property_data() %>%
      mutate(popup = paste("<center> <b>Address </b> <br>", address, "</center>")) %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>%
      addMarkers(
        lng = ~long,
        lat = ~lat,
        label = ~property_id,
        popup = ~popup
      )
  })
  # Create Datatable
  output$propertyDT <- renderDT({
    datatable(reactive_property_data(),
      options = list(
        scrollY = "45vh", scrollX = "150%",
        autoWidth = TRUE,
        columnDefs = list(list(width = "300px", targets = c(9)))
      )
    ) %>%
      formatCurrency(columns = c(6))%>%
      formatStyle(0, lineHeight = "50%")
  })
  # Plot data
  output$property_scatter <- renderEcharts4r({
    reactive_property_data() %>%
      group_by(property_type) %>%
      e_charts(property_area) %>%
      e_scatter(property_price, name = "Property Price", symbol_size = 15, color = "purple") %>%
      e_tooltip() %>%
      e_legend(show = FALSE) %>% 
      e_axis_labels(y = "Price", x = "ID")%>%
      e_labels(
        position = "top"
        , color = "#111"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("function(params){ 
                                      return('$' + parseInt(params.value[1]/1000000) + 'M')
                                      }")) %>%
      e_tooltip(
        position = "right"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("
             function(params){
              var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});
              
               return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Property Price: ' + formatter.format(params.value[1]/ 1000000)+ 'M')
            }")
      ) %>%
      e_y_axis(formatter = e_axis_formatter(style = "currency")) %>% 
      e_x_axis(axisLabel = list(interval = 1)) 
    # browser()
  })

  output$property_bar <- renderEcharts4r({
    reactive_property_data() %>%
      # group_by(property_type)%>%
      e_charts(property_id) %>%
      e_bar(count_bed, symbol_size = 15, color = "purple") %>%
      e_legend(show = F) %>%
      e_tooltip(
        position = "right"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("
             function(params){
              var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});
              
               return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Bedroom Count: ' + params.value[1])
            }")) %>%
      e_axis_labels(y = "Bedroom Count", x = "ID")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
