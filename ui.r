library(shiny)
fluidPage(
  titlePanel("YetAnoterProject"),
  radioButtons("Languages", label = h3("Language"),
               choices = list("English" = 1, "Russian" = 2),
               selected = 2),
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "User",
      sidebarPanel(
        selectInput("programminglanguage", label = h3("Select Programming Language"), 
                    choices = list("Pascal" = 1, "python" = 2, "C++" = 3), 
                    selected = 1),
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Task A",
            fileInput("fileA", label = h3("Put Your Code Here")),
            actionButton("actionUA", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task B",
            fileInput("fileB", label = h3("Put Your Code Here")),
            actionButton("actionUB", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task C",
            fileInput("fileC", label = h3("Put Your Code Here")),
            actionButton("actionUC", label = "Check"),
            hr()
          )
        ),
      ),
    ),
    tabPanel(
      "Admin",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Task A",
          mainPanel(
            actionButton("actionAA", label = "Add description"),
            hr()
          )
        ),
        tabPanel(
          "Task B",
          mainPanel(
            actionButton("actionAB", label = "Add description"),
            hr()
          )
        ),
        tabPanel(
          "Task C",
          mainPanel(
            actionButton("actionAC", label = "Add description"),
            hr()
          )
        )
      )
    )
  )
)
