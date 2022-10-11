library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(echarts4r)
library(leaflet)
# setwd("C:/Users/Admin/OneDrive - Charité - Universitätsmedizin Berlin/edx-Intro-to-RShiny/park")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Park App"),
  # Sidebar -----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        tabName = "species",
        text = "Species"
      ),
      menuItem(
        tabName = "map",
        text = "Map"
      ),
      menuItem(
        tabName = "journal",
        text = "Journal"
      )
    )
  ),
  # body -----
  dashboardBody(
    includeCSS("www/style.css"),
    tags$head(tags$style(HTML(".small-box {height: 90px}"))),
    # tags$head(tags$style(HTML(".small-box {height: 100px}"))),
    tabItems(
      ## Species Tab -----
      tabItem(
        tabName = "species",
        fluidPage(
          fluidRow(
            box(
              width = 4,
              status = "primary",
              title = "Filter Parks and Species",
              height = "50vh",
              uiOutput("park_picker"),
              uiOutput("species_select")
            ),
            box(
              width = 4,
              status = "primary",
              title = "Species Overview",
              height = "50vh",
              valueBoxOutput("spec_present",
                width = 12
              ),
              fluidRow(
                valueBoxOutput("spec_concerned",
                  width = 6
                ),
                valueBoxOutput("spec_endangered",
                  width = 6
                )
              )
            ),
            box(
              width = 4,
              status = "primary",
              title = "Category count",
              height = "50vh",
              echarts4rOutput("category_count")
            )
          ),
          fluidRow(
            box(
              width = 12,
              height = "30vh",
              status = "primary",
              # style = 'overflow-x: scroll;height:200px;overflow-y: scroll;',
              DTOutput("spec_table")
            )
          )
        )
      ),
      ## map tab -----
      tabItem(
        tabName = "map",
        fillPage(
          fluidPage(
            fluidRow(
              box(
                width = 12,
                status = "primary",
                chooseSliderSkin(skin = "Shiny", color = "green"),
                uiOutput("area_slider")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                height = "60vh",
                leafletOutput("map",
                  height = "55vh"
                )
              )
            )
          )
        )
      ),
      ## journal tab-----
      tabItem(
        tabName = "journal",
        fluidPage(
          fluidRow(
            box(width = 12)
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  parks <- read_csv("data/parks.csv")
  park_header <- colnames(parks)
  parks <- janitor::clean_names(parks)
  species <- read_csv("data/species.csv")
  species_header <- colnames(species)
  species <- janitor::clean_names(species)

  # species table -----
  ## filters -----
  output$park_picker <- renderUI({
    choices <- parks$park_name
    pickerInput("park_picker", "Select Park",
      choices = choices
    )
  })

  output$species_select <- renderUI({
    req(input$park_picker)
    categories <- species %>%
      filter(park_name == input$park_picker) %>%
      select(category) %>%
      pull() %>%
      unique()
    categories <- categories[categories != ""]
    checkboxGroupButtons(
      "category_check_box", "Species Category",
      choices = categories,
      selected = categories,
      status = "default",
      size = "xs",
      # direction = "vertical",
      width = "80%"
    )
  })






  ## reactive species-----
  reactive_species <- reactive({
    req(input$park_picker)
    species %>% filter(
      park_name == input$park_picker,
      category %in% input$category_check_box
    )
  })

  ## species output-----
  ### species table-----
  output$spec_table <- renderDT({
    reactive_species() %>%
      select(
        "Common Name" = common_names,
        "Scientific Name" = scientific_name,
        "Category" = category,
        "Occurrence" = occurrence,
        "Nativeness" = nativeness,
        "Abundance" = abundance,
        "Conservation Status" = conservation_status
      ) %>%
      datatable(
        rownames = FALSE,
        width = "100%",
        height = "25vh",
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "30vh",
          searching = FALSE,
          info = FALSE
        )
      )
  })
  ### species value boxes -----

  output$spec_present <- renderValueBox({
    present <- reactive_species() %>%
      filter(occurrence == "Present") %>%
      nrow()
    valueBox(
      value = tags$p(format(present, big.mark = ","),
        style = "font-size: 80%;"
      ),
      subtitle = tags$p("Present Species",
        style = "font-size: 90%;"
      ),
      color = "green"
    )
  })

  output$spec_concerned <- renderValueBox({
    present <- reactive_species() %>%
      filter(conservation_status == "Species of Concern") %>%
      nrow()
    valueBox(
      value = tags$p(format(present, big.mark = ","),
        style = "font-size: 60%;"
      ),
      subtitle = tags$p("Species of Concern",
        style = "font-size: 80%;"
      ),
      width = 12,
      color = "orange"
    )
  })

  output$spec_endangered <- renderValueBox({
    present <- reactive_species() %>%
      filter(conservation_status == "Endangered") %>%
      nrow()
    valueBox(
      value = tags$p(format(present, big.mark = ","),
        style = "font-size: 60%;"
      ),
      subtitle = tags$p("Endangered Species",
        style = "font-size: 80%;"
      ),
      width = 12,
      color = "red"
    )
  })
  ### plot ------
  output$category_count <- renderEcharts4r({
    reactive_species() %>%
      select(species_id, category) %>%
      group_by(category) %>%
      table() %>%
      as.data.frame() %>%
      group_by(category) %>%
      summarise(count = sum(Freq)) %>%
      e_chart(category) %>%
      e_bar(count,
        name = "Count Category",
        color = "green"
      ) %>%
      e_legend(show = FALSE) %>%
      e_tooltip() %>%
      e_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
      e_grid(
        top = "1%",
        left = "20%",
        height = "30%"
      )
  })
  # map tab -----
  ## slider input -----
  output$area_slider <- renderUI({
    max <- max(parks$acres)
    min <- min(parks$acres)
    sliderInput("area_slider",
      "Select area of National Parl",
      min = min,
      max = max,
      value = c(min, max),
      post = "acres"
    )
  })

  ## map -----
  reactive_park <- reactive({
    req(input$area_slider)
    parks %>% filter(acres >= input$area_slider[1] &
      acres <= input$area_slider[2])
  })


  output$map <- renderLeaflet({
    reactive_park() %>%
      mutate(
        acres_f = format(acres, big.mark = " "),
        popup = paste(
          "<b>", park_name, "</b>",
          "<br>", state, "<br>", acres_f,
          "acres"
        )
      ) %>%
      leaflet() %>%
      addProviderTiles(provider = "OpenTopoMap") %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ acres / 200000,
        label = ~park_name,
        popup = ~popup
      )
  })
}
# Run the application
shinyApp(ui = ui, server = server)
