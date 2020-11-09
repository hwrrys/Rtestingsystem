library(shiny)
fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "User",
        tabsetPanel(
        type = "tabs",
          tabPanel(
          "Task A",
          fileInput("fileu", label = h3("Put Your Code Here")),
          radioButtons("Languagesu", label = h3("Language"),
                       choices = list("English" = 1, "Russian" = 2),
                       selected = 2),
          selectInput("programminglanguagea", label = h3("Select "), 
                      choices = list("Task A" = 1, "Task B" = 2, "Task C" = 3), 
                      selected = 1),
          selectInput("programminglanguageu", label = h3("Select programming language"), 
                      choices = list("Pascal.ABC" = 1, "Python 3" = 2, "C++" = 3), 
                      selected = 1),
          actionButton("actionu", label = "Check"),
          hr()
        ),
        tabPanel(
          "Task B",
          fileInput("fileu", label = h3("Put Your Code Here")),
          radioButtons("Languagesu", label = h3("Language"),
                       choices = list("English" = 1, "Russian" = 2),
                       selected = 2),
          selectInput("programminglanguagea", label = h3("Select "), 
                      choices = list("Task A" = 1, "Task B" = 2, "Task C" = 3), 
                      selected = 1),
          selectInput("programminglanguageu", label = h3("Select programming language"), 
                      choices = list("Pascal.ABC" = 1, "Python 3" = 2, "C++" = 3), 
                      selected = 1),
          actionButton("actionu", label = "Check"),
          hr()
        ),
        tabPanel(
          "Task C",
          fileInput("fileu", label = h3("Put Your Code Here")),
          radioButtons("Languagesu", label = h3("Language"),
                       choices = list("English" = 1, "Russian" = 2),
                       selected = 2),
          selectInput("programminglanguagea", label = h3("Select "), 
                      choices = list("Task A" = 1, "Task B" = 2, "Task C" = 3), 
                      selected = 1),
          selectInput("programminglanguageu", label = h3("Select programming language"), 
                      choices = list("Pascal.ABC" = 1, "Python 3" = 2, "C++" = 3), 
                      selected = 1),
          actionButton("actionu", label = "Check"),
          hr()
        )
      ),
    ),
    tabPanel(
      "Admin",
      tabsetPanel(
      type = "tabs",
        tabPanel(
        "Task A",
        radioButtons("Languagesa", label = h3("Language"),
                     choices = list("English" = 1, "Russian" = 2),
                     selected = 2),
        actionButton("actiona", label = "Add description"),
        hr()
        ),
        tabPanel(
          "Task B",
          radioButtons("Languagesa", label = h3("Language"),
                       choices = list("English" = 1, "Russian" = 2),
                       selected = 2),
          actionButton("actiona", label = "Add description"),
          hr()
        ),
        tabPanel(
          "Task C",
          radioButtons("Languagesa", label = h3("Language"),
                       choices = list("English" = 1, "Russian" = 2),
                       selected = 2),
          actionButton("actiona", label = "Add description"),
          hr()
        )
      )
    )
  )
)