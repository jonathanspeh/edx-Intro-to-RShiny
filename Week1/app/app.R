#
# This is a Shiny web application created in the first week of the course. 
# The basic app is used and functions are replaced by shinydashbord functions





# Dependencies
library(shiny)
library(shinydashboard)


# dashboardHeader(   # replaces titlePanel(
#   title = "Old Faithful Geyser Data"),
# dashboardSidebar( # replaces sidebarLayout(
#   sidebarMenu(    # replaces sidebarPanel(
#     sliderInput("bins",
#                 "Number of bins:",
#                 min = 1,
#                 max = 50,
#                 value = 30)
#   )),
# 
# Show a plot of the generated distribution
# dashboardBody(    # replaces mainPanel(
#   plotOutput("distPlot")
# )



# Define UI for application that draws a histogram
ui <- dashboardPage(   #replaces fluidPage(
        dashboardHeader(   # replaces titlePanel(
          title = "Old Faithful Geyser Data"),
        dashboardSidebar(
          sidebarMenu(
            menuItem(tabName = "data", text = "Data",
                     icon = icon("chart-simple")),
            menuItem(tabName = "history", text = "History",
                     icon = icon("clock-rotate-left"))
          )
        ),
        dashboardBody(    # replaces mainPanel(
          tabItems(
            tabItem(tabName = "data",
                    fluidRow(column(width = 4,
                 box(width = 12,
                     title = "slider Input",
                     status = "primary",
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)))),
          fluidRow(
            column(width = 8,
                 box(width = 12,
                     title = "Histogram",
                     status = "danger",
                     background = "purple",
                     collapsible = TRUE,
            plotOutput("distPlot")))
        )),
        tabItem(tabName = "history",
                box(width = 12,
                    title = "History of the OFG",
                    status = "info",
                    "Here you find information about the OFG: https://en.wikipedia.org/wiki/Old_Faithful"))
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
