# Script to try functions while creating app

library(styler)

here::here("Week3","Week3_myOwn","cat.jpg")



jpeg::readJPEG("C:/Users/Admin/OneDrive - Charité - Universitätsmedizin Berlin/edx-Intro-to-RShiny/Week3/Week3_myOwn/cat.jpg")



ui <- fluidPage(
  imageOutput("plot3")
)

server <- function(input, output) {
  # Send a pre-rendered image, and don't delete the image after sending it
  # NOTE: For this example to work, it would require files in a subdirectory
  # named images/
  output$plot3 <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    #filename <- normalizePath(file.path("C:/Users/Admin/OneDrive - Charité - Universitätsmedizin Berlin/edx-Intro-to-RShiny/Week3/Week3_myOwn/cat.jpg"))
    filename <- normalizePath(file.path(here::here("Week3","Week3_myOwn","cat.jpg")))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
}

shinyApp(ui, server)



#here::here("Week3","Week3_myOwn","cat.jpg")

propertyTable|>
  filter(property_type %in% c("lot", "house"),
         property_price <= 5000000,
         count_bed >= 3)

dim(propertyTable)
propertyTable$lat

propertyTable%>%
  leaflet()%>%
  addTiles()%>%
  addMarkers(lng = propertyTable$long,
             lat = propertyTable$lat,
             label = propertyTable$property_id,
             #popup = ~popup
  )



ui <- dashboardPage(
  dashboardHeader(title = "Week 3 - Simple App"),
  # Sidebar
  dashboardSidebar(),
  # Body
  dashboardBody(
    fluidRow(
      box(
        width = 6, title = "Inputs"),
      box(
        width = 6, title = "Spaceholder",
        plotOutput("myImage"))),
    fluidRow(
      box(
        width = 6, title = "Inputs"),
      box(
        width = 6, title = "Spaceholder")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Placeholder image
  output$myImage = renderImage({ 
    filename <- normalizePath(file.path(here::here("Week3","Week3_myOwn","cat.jpg")))
    list(src = filename)}, 
    deleteFile = FALSE)
  
}

# Run the application
shinyApp(ui = ui, server = server)



#gs4_auth(email = "jspeh456@gmail.com") # opens authentification Browser , once verified, enter a-mail
#propertyTable <- range_read(ss = "https://docs.google.com/spreadsheets/d/1u_2Nn_ZeN0VdEGYJewQ2bJd_1KZzzjjUpQLwZ6_r8Uw/edit#gid=0")
#propertyTable$property_area <- unlist(propertyTable$property_area)

  
  propertyTable|>
  filter(property_type %in% c("lot", "house"),
         property_price <= 5000000,
         count_bed >= 3)

dim(propertyTable)
propertyTable$lat

property_table%>%
  leaflet()%>%
  addTiles()%>%
  addMarkers(lng = ~long,
             lat = ~lat,
             label = propertyTable$property_id,
             #popup = ~popup
  )

propertyTable$property_type

propertyTable%>%
  group_by(property_type)%>%
  e_charts(property_area)%>%
  e_scatter(property_price, symbol_size = 15)%>%
  e_tooltip()

datatable(propertyTable,
          options = list(scrollY = "25vh", scrollX = "100%",
                         autoWidth = TRUE, 
                         columnDefs = list(list(width = '300px', targets = c(9)))))%>%
            formatStyle(0,lineHeight='60%')
          





dir.create(here::here("Week3", "Week3_myOwn", "www"))

propDat = read_csv(here::here("Week3", "Week3_myOwn", "www", "property_data.csv"))
save(propDat, file = here::here("Week3", "Week3_myOwn", "propDat.RData"))
load(file = here::here("Week3", "Week3_myOwn", "propDat.RData"))

mpd = propDat
