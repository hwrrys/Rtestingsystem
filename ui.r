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
#            fluidRow(column(2, verbatimTextOutput("value"))),
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
          ),
          tabPanel(
            "Task D",
            fileInput("fileD", label = h3("Put Your Code Here")),
            actionButton("actionUD", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task E",
            fileInput("fileE", label = h3("Put Your Code Here")),
            actionButton("actionUE", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task F",
            fileInput("fileF", label = h3("Put Your Code Here")),
            actionButton("actionUF", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task G",
            fileInput("fileG", label = h3("Put Your Code Here")),
            actionButton("actionUG", label = "Check"),
            hr()
          ),
          tabPanel(
            "Task Z",
            fileInput("fileZ", label = h3("Put Your Code Here")),
            actionButton("actionUZ", label = "Check"),
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
          sidebarPanel(
            fileInput("descriptionA", label = "Description here"),
            actionButton("actionAA", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task B",
          sidebarPanel(
            fileInput("descriptionB", label = "Description here"),
            actionButton("actionAB", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task C",
          sidebarPanel(
            fileInput("descriptionC", label = "Description here"),
            actionButton("actionAC", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task D",
          sidebarPanel(
            fileInput("descriptionD", label = "Description here"),
            actionButton("actionAD", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task E",
          sidebarPanel(
            fileInput("descriptionE", label = "Description here"),
            actionButton("actionAE", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task F",
          sidebarPanel(
            fileInput("descriptionF", label = "Description here"),
            actionButton("actionAF", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task G",
          sidebarPanel(
            fileInput("descriptionG", label = "Description here"),
            actionButton("actionAG", label = "Add description"),
            hr()
          ),
        ),
        tabPanel(
          "Task Z",
          sidebarPanel(
            fileInput("descriptionZ", label = "Description here"),
            actionButton("actionAZ", label = "Add description"),
            hr()
          ),
        )
      )
    )
  )
)
