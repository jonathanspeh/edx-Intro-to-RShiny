# UI for app to show plots of mpg data as well as two generated, 
#random normally distributed data sets. 
# Uses nested tab boxes

source(here::here("MultifileApp", "Helper.R"))

shinyUI(dashboardPage(
  dashboardHeader(title = "Title of my Dashboard"),
  dashboardSidebar( # Sidebar used to select parameters for generated Data sets 
    fluidRow(column(6,
                    numericInput(inputId = "meanID1",
                                 label = "mean of Dataset 1",
                                 value = 0)),
             column(6,
                    numericInput("sdID1",
                                 "Sd of Dataset 1",
                                 1))),
    br(),
    fluidRow(column(6,
                   numericInput(inputId = "meanID2",
                                label = "mean of Dataset 2",
                                value = 0)),
            column(6,
                   numericInput("sdID2",
                                "Sd of Dataset 2",
                                1))),
    br(),
    fluidRow(
    actionBttn("submit", "submit Input") # must be pressed, Plot is wrapend in observe event
    )),
  dashboardBody(
    fluidRow(
      tabBox(width = 12,
             tabPanel(
      tabBox(width = 12, # tabbox to select properties of mpg plot
             tabPanel(selectInput("myX", label = h3("Select X-Variable"),
                                  choices = numVars,
                                  selected = 1),
                      selectInput("myY", label = h3("Select Y-Variable"),
                                  choices = numVars,
                                  selected = numVars[2]),
                      selectInput("myClas", label = h3("Select class to display"),
                                  choices = classVars,
                                  selected = 1),
                      selectInput("myCol", label = h3("Select property to colour dots"),
                                  choices = textVars,
                                  selected = 1),
                      title = "Inputs"),
             tabPanel(plotOutput("myPlot"), # shows mpg plot
                      title = "plot"),
             tabPanel(dataTableOutput("myTable"),# show selected data
                      title = "Selected X and Y data")),
      title = "mpg plot"),
      tabPanel(title = "Your own data",
               tabBox(width = 12,
                      tabPanel(title = "Datatable output", # show generated Data
                               dataTableOutput("yourTable")),
                      tabPanel(title = "Your plot!",  # show corresponding Plot
                               plotOutput("yourPlot")))))))))
               
             


