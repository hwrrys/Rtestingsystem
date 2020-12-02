library(shiny)
a <- 0
fluidPage(
  titlePanel("YetAnoterProject"),
  radioButtons("Languages", label = h3("Language"),
               choices = list("English" = 1, "Russian" = 2),
               selected = 2),
  uiOutput("ui"),
  actionButton("Login", label = "Login/Logout")
)