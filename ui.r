library(shiny)
fluidPage(
  titlePanel("YetAnoterProject"),
  radioButtons("Languages", label = h3("Language"),
               choices = list("English" = 1, "Russian" = 2),
               selected = 2),
  h4("if you need to register enter Login: 'reg'"),
  textInput("login","Login:"),
  textInput("password","Password:"),
  actionButton("Login", label = "Login"),
  uiOutput("ui") 
)
