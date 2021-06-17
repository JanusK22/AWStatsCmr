


library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ####Pop up####
  shinyalert(
    title = "Welcome",
    text = "<< For Tabs - For Theming >>",
    size = "xs", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#5D3FD3",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  output$table <- renderReactable({
    reactable(ins_datas,showPageSizeOptions = TRUE,showPagination = TRUE,
              striped = TRUE,defaultPageSize = 7,paginationType = "jump",
              highlight = TRUE,filterable = TRUE,groupBy = c("Area" ),theme =reactableTheme( 
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )) 
  })
  
  
  
  ### Output Region####
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
  
  ###### Download #######
  
  output$downloadData1 <- downloadHandler(
    filename <- function() {
      paste("script", "Rmd", sep=".")
    },
    
    content <- function(file) {
      file.copy("s.Rmd", file)
    })
  
  output$downloadData2 <- downloadHandler(
    filename <- function() {
      paste("ui", "R", sep=".")
    },
    
    content <- function(file) {
      file.copy("ui.R", file)
    })
  
  output$downloadData3 <- downloadHandler(
    filename <- function() {
      paste("server", "R", sep=".")
    },
    
    content <- function(file) {
      file.copy("server.R", file)
    })
  
  
  
})
