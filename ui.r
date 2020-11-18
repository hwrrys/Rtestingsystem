  library(shiny)
  fluidPage(
    titlePanel("YetAnoterProject"),
    radioButtons("Languages", label = h3("Language"),
                 choices = list("English" = 1, "Russian" = 2),
                 selected = 2),
    textInput("password","Password"),
    uiOutput("ui") 
  )
