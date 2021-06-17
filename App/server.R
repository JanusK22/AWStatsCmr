


library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ### Output Region
  output$plotregion <- renderPlot({
    print(g(input$variable , input$radiotheme))
    
    if (input$compareregion == "TRUE") {
      output$plotregion2 <- renderPlot({
        print(g(input$variable2 , input$radiotheme))
      })
    }
    else{
      output$plotregion2 <- renderPlot({
        
      })
    }
  })
  
  text = paste("\n   Please select at Least One Variable.\n")
  
  ###Output Line
  output$line <- renderPlotly({
    if (length(input$checkline) == 0) {
      ggplotly(
        ggplot() +
          annotate(
            "text",
            colour = "blue",
            x = 4,
            y = 25,
            size = 8,
            label = text
          ) +
          theme_void()
      )
    }
    
    else{
      ggplotly(line(input$checkline))
    }
    
  })
  
  ###Output Line
  output$bar <- renderPlotly({
    if (length(input$checkbar) == 0) {
      ggplotly(
        ggplot() +
          annotate(
            "text",
            colour = "purple",
            x = 4,
            y = 25,
            size = 8,
            label = text
          ) +
          theme_void()
      )
    }
    
    else{
      ggplotly(bar(input$checkbar,input$bartheme))
    }
    
  })
  
})
