# Server for data / plot app


shinyServer(function(input, output) {
  # render table of selected data from mpg
  output$myTable <- renderDT({ 
    myData %>% select(input$myX, input$myY)})
 
  # generate and render plot of selected data from mpg
   output$myPlot = renderPlot({myData %>% filter(class == input$myClas) %>%
    ggplot(aes_(x = as.name(input$myX), y = as.name(input$myY))) +
      geom_smooth(method = "lm", formula = y~x) + 
      geom_point(aes_(color = as.name(input$myCol)))})
   
   # observe action button, generate data sets, datatable and corresponding plot
  observeEvent(input$submit,{
    ds1 = rnorm(100, input$meanID1, input$sdID1)
    ds2 = rnorm(100, input$meanID2, input$sdID2)
    yourTable = tibble(ds1, ds2)
    output$yourTable = renderDT({yourTable})
    output$yourPlot = renderPlot({yourTable%>%pivot_longer(cols = c(1,2))%>%
        ggplot(aes(x = name, y = value)) +
          geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
          geom_point(position = "jitter")})})})
    
  
  #ds1 = rnorm(100, input$meanID1, input$sdID1)
   # ds2 = rnorm(100, input$meanID2, input$sdID2)
# myDat%>%pivot_longer(c(1,2))%>%
#   ggplot(aes(x = name, y = value)) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
#   geom_point(position = "jitter")
