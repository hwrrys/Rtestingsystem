library(shiny)

a <<- 0
b <<- 0
rez <<- ''
nor <<- 0
fluidPage(
  #style="color: #ff1; background-color: #f1f; border-color: #fff",
  titlePanel("YetAnoterProject"),
  radioButtons("Languages", label = h3("Language"),
               choices = list("English" = 1, "Russian" = 2),
               selected = 2),
  mainPanel(
    actionButton("registery", label = "I need to register"),
    actionButton("Login", label = "Login/Logout", icon("paper-plan"))
  ),
  #style="color: #fff; background-color: #fff; border-color: #fff")
  uiOutput("ui")
)