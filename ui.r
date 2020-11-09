library(shiny)
fluidPage(
  
  # Copy the line below to make a file upload manager
  fileInput("file", label = h3("Put Your Code Here")),
  radioButtons("Languages", label = h3("Language"),
               choices = list("English" = "English", "Russian" = "Russian"), 
               selected = 1),
  selectInput("programminglanguage", label = h3("Select programming language"), 
              choices = list("Pascal.ABC" = 1, "Python 3" = 2, "C++" = 3), 
              selected = 1),
  
  actionButton("action", label = "Check"),
  
  hr()
)