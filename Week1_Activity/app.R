library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# # Define UI for Activity 1 - 2 boxes -----
# ui <- dashboardPage(dashboardHeader(title = "Activity 1"),
#                     dashboardSidebar(disable = TRUE),
#                     dashboardBody(
#                       fluidRow(
#                         box(width = 6, title = "Box 1 - Inputs",
#                             sliderInput("bins",
#                                         "Number of bins in Histogram:",
#                                         min = 1,
#                                         max = 50,
#                                         value = 30)),
#                         box(width = 6, title = "Box 2",
#                             plotOutput("distPlot")))))


# # Define UI for exercise 2 - 4 boxes on two rows -----
# ui <- dashboardPage(dashboardHeader(title = "Activity 1"),
#                     dashboardSidebar(disable = TRUE),
#                     dashboardBody(
#                       fluidRow(title = "Top Row",
#                                box(width = 6, title = "Box 1 - Inputs",
#                                     sliderInput("bins",
#                                                 "Number of bins in Histogram:",
#                                                 min = 1,
#                                                 max = 50,
#                                                 value = 30),
#                                    selectInput("colour",
#                                                label = "Select colour of lineplot",
#                                                choices = c("red","green","blue","black"))),
#                                box(width = 6, title = "Box 2",
#                                     plotOutput("distPlot"))),
#                       fluidRow(title = "Bottom Row",
#                                box(width = 6, title = "Box 3 - Lineplot",
#                                    plotOutput("linePlot")),
#                                box(width = 6, title = "Box 4",
#                                    DTOutput("dataTable")))))

# # Define UI for exercise 3 - 6 boxes on two rows -----
# ui <- dashboardPage(dashboardHeader(title = "Activity 1"),
#                     dashboardSidebar(disable = TRUE),
#                     dashboardBody(
#                       fluidRow(title = "Top Row",
#                                box(width = 6, title = "Box 1 - Lineplot",
#                                    plotOutput("linePlot")),
#                                box(width = 6, title = "Box 2 - dist Plot",
#                                    plotOutput("distPlot"))),
#                       fluidRow(title = "Bottom Row",
#                                box(width = 3, title = "Box 3 - Inputs",
#                                    sliderInput("bins",
#                                                "Number of bins in Histogram:",
#                                                min = 1,
#                                                max = 50,
#                                                value = 30),
#                                    selectInput("colour",
#                                                label = "Select colour of lineplot",
#                                                choices = c("red","green","blue","black"))),
#                                box(width = 3, title = "Box 4 - Data Table",
#                                    DTOutput("dataTable")),
#                                box(width = 3, title = "Box 5",
#                                ),
#                                box(width = 3, title = "Box 6"))))

# Define UI for exercise 4 - previous Layouts as tabs -----
ui <- dashboardPage(dashboardHeader(title = "Activity 1"),
                    dashboardSidebar(sidebarMenu(
                      menuItem(tabName = "2Box", text = "2 Two Box Layout", icon = icon("chart-bar")), 
                      menuItem(tabName = "4Box", text = "4 Two Box Layout", icon = icon("folder-open")),
                      menuItem(tabName = "6Box", text = "6 Two Box Layout", icon = icon("folder-open")))),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "2Box",
                                fluidRow(
                                  box(width = 6, title = "Box 1"),
                                  box(width = 6, title = "Box 2",))),
                        tabItem(tabName = "4Box",
                                fluidRow(title = "Top Row",
                                         box(width = 6, title = "Box 1"),
                                         box(width = 6, title = "Box 2")),
                                fluidRow(title = "Bottom Row",
                                         box(width = 6, title = "Box 3"),
                                         box(width = 6, title = "Box 4"))),
                        tabItem(tabName = "6Box",
                                       fluidRow(title = "Top Row",
                                                box(width = 6, title = "Box 1"),
                                                box(width = 6, title = "Box 2")),
                                       fluidRow(title = "Bottom Row",
                                                box(width = 3, title = "Box 3"),
                                                box(width = 3, title = "Box 4"),
                                                box(width = 3, title = "Box 4"),
                                                box(width = 3, title = "Box 4")
                                                )))))
dashboardPage(
  
)


# Define server logic for all the four apps-----
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
    output$linePlot = renderPlot({
      faithful%>%
        ggplot(aes(x=waiting,y=eruptions))+
        geom_line(colour = input$colour)
    })
    output$dataTable = renderDT(faithful)
}

# Run the application 
shinyApp(ui = ui, server = server)
