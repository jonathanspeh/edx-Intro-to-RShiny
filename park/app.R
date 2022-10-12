library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(echarts4r)
library(leaflet)
library(googlesheets4)

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
              height = "45vh",
              uiOutput("park_picker"),
              uiOutput("species_select")
            ),
            box(
              width = 4,
              status = "primary",
              title = "Species Overview",
              height = "45vh",
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
              height = "45vh",
              echarts4rOutput("category_count")
            )
          ),
          fluidRow(
            box(
              width = 12,
              height = "35vh",
              status = "primary",
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
            box(
              width = 12, status = "primary",
              height = "80vh",
              title = "Trip Journal",
              actionBttn("add_entry", icon = icon("plus")),
              DTOutput("journal")
            )
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
          scrollY = "20vh",
          searching = FALSE,
          info = FALSE
        )
      ) %>%
      formatStyle("Conservation Status",
        target = "row",
        backgroundColor = styleEqual(c(NA, "Species of Concern", "Endangered"), c(NA, "orange", "red"))
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
        top = "4%",
        left = "20%",
        height = "25%"
      )
  })
  # map tab -----
  ## slider input -----
  output$area_slider <- renderUI({
    max <- max(parks$acres)
    min <- min(parks$acres)
    sliderInput("area_slider",
      "Select Area of National Park",
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
  # Journal tab -----
  ## authentification -----
  options(gargle_oauth_cache = ".secrets") # designate project-specific cache
  # gargle::gargle_oauth_cache() # check the value of the option
  # googlesheets4::gs4_auth()# trigger auth on purpose to store a token in the specified cache
  cache_directory <- ".secrets/" # can add to config file
  # list.files(cache_directory) # see your token file in the cache
  # googlesheets4::gs4_deauth() # de auth

  gs4_auth(email = "jspeh456@gmail.com", cache = cache_directory)
  journal_url <- "https://docs.google.com/spreadsheets/d/1gJ4dRZDwxnptrgscCvH3fu0ns9kzcRcZwpPDN7dnGvM/edit#gid=0"
  park_journal <- range_read(ss = journal_url)
  r <- reactiveValues()
  r$park_journal <- park_journal


  ## datatable -----
  output$journal <- renderDT({
    datatable(r$park_journal,
      rownames = FALSE,
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "50vh",
        searching = FALSE,
        info = FALSE
      )
    )
  })


  observeEvent(input$add_entry, {
    choices <- parks$park_name
    showModal(
      modalDialog(
        title = "New Trip",
        footer = fluidRow(
          column(
            width = 6,
            actionBttn("save", icon = icon("save"))
          ),
          column(
            width = 6,
            actionBttn("dismiss", icon = icon("times"))
          )
        ),
        easyClose = TRUE,
        fluidRow(
          column(
            width = 6,
            textInput("name", "Trip Name", value = "Trip Name"),
            dateInput("date", "Trip Date",
              format = "dd. MM yyyy"
            )
          ),
          column(
            width = 6,
            pickerInput("park_name", "Park Name",
              choices = choices
            ),
            textAreaInput("notes", "Notes", value = "Notes...")
          )
        )
      )
    )
  })
  observeEvent(input$save, {
    trip_name <- input$name
    trip_date <- format(input$date, "%d. %B %Y")
    park_name <- input$park_name
    notes <- input$notes
    new_entry <- tibble(
      "Trip Name" = trip_name,
      "Trip Date" = trip_date,
      "Park Name" = park_name,
      "Notes" = notes
    )
    sheet_append(
      ss = journal_url,
      data = new_entry
    )
    showNotification("Trip added", type = "message")
    r$park_journal <- rbind(r$park_journal, new_entry)
    removeModal()
  })
  observeEvent(input$dismiss, {
    removeModal()
  })
}
# Run the application
shinyApp(ui = ui, server = server)
