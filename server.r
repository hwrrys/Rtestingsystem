library(shiny)
library(plotly)
function(input, output){
  
  # You can access the value of the widget with input$file, e.g.
  output$value <- renderPrint({
    str(input$file)
  output$value <- renderPrint({ input$language
    })
  })
}