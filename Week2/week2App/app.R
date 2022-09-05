#App for Week 2
# Based on 4 Box template

# Dependencies
library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(magrittr)
library(shinyWidgets)


##### UI 
ui <- dashboardPage(
  dashboardHeader(
    title = "Week Two"
  ),
  dashboardSidebar(disable = T
  ),
  dashboardBody(
    fluidRow(
      box(width = 6,
          selectInput("select", label = h3("Select box"), 
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                      selected = 1),
          
          hr(),
          fluidRow(column(3, verbatimTextOutput("value"))),
          br(),
          sliderInput("slider1", label = h3("Slider"), min = 0, 
                      max = 100, value = 50),
          br(),
          fluidRow(column(3, verbatimTextOutput("sliderValue"))),
          br(),
          textInput("text", label = h3("Text input"), value = "Enter text..."),
          
          hr(),
          verbatimTextOutput("textValue")),
      box(width = 6, title = "Iris Datatable", status = "info",
          DTOutput("datatable"))), 
    fluidRow(
      box(width = 6,
          leafletOutput("myMap")), 
      tabBox(width = 6,
             tabPanel(title = "barplot - badplot",
                      echarts4rOutput("barchart")),
             tabPanel(title = "scatterplot - goodplot",
                      echarts4rOutput("scatterplot")))
    )
  ),
  title = "Dashboard example"
)


# Server ----
server <- function(input, output) {
  output$datatable = renderDT({
    datatable(iris)
  })
  df <- data.frame(
    x = seq(50),
    y = rnorm(50, 10, 3),
    z = rnorm(50, 11, 2),
    w = rnorm(50, 9, 2))
  output$barchart = renderEcharts4r(df |> 
                                      e_charts(x) |> 
                                      e_bar(y, name = "Serie 1") |> 
                                      e_step(z, name = "Serie 2") |> 
                                      e_title("Bar and step charts"))
  output$scatterplot = renderEcharts4r(df |> 
                                         e_charts(x) |> 
                                         e_scatter(y, z) |> 
                                         e_visual_map(z, scale = e_scale) |> # scale color
                                         e_legend(FALSE))
  output$myMap = renderLeaflet(leaflet() |>
                               addTiles() |> 
                               addMarkers(lng=174.768, 
                                          lat=-36.852, 
                                          popup="The birthplace of R"))
  output$value <- renderPrint({ input$select })
  output$sliderValue <- renderPrint({ input$slider1 })
  output$textValue <- renderPrint({ input$text })
}

# Run the application 
shinyApp(ui = ui, server = server)
