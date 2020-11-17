library(shiny)
library(readtext)
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
                    selected = 2),
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Task A",
            verbatimTextOutput("valueA"),
            fileInput("fileA", label = h3("Put Your Code Here")),
            actionButton("actionUA", label = "Check"),
            tableOutput('UA'),
            hr()
          ),
          tabPanel(
            "Task B",
            verbatimTextOutput("valueB"),
            fileInput("fileB", label = h3("Put Your Code Here")),
            actionButton("actionUB", label = "Check"),
            tableOutput('UB'),
            hr()
          ),
          tabPanel(
            "Task C",
            verbatimTextOutput("valueC"),
            fileInput("fileC", label = h3("Put Your Code Here")),
            actionButton("actionUC", label = "Check"),
            tableOutput('UC'),
            hr()
          ),
          tabPanel(
            "Task D",
            verbatimTextOutput("valueD"),
            fileInput("fileD", label = h3("Put Your Code Here")),
            actionButton("actionUD", label = "Check"),
            tableOutput('UD'),
            hr()
          ),
          tabPanel(
            "Task E",
            verbatimTextOutput("valueE"),
            fileInput("fileE", label = h3("Put Your Code Here")),
            actionButton("actionUE", label = "Check"),
            tableOutput('UE'),
            hr()
          ),
          tabPanel(
            "Task F",
            verbatimTextOutput("valueF"),
            fileInput("fileF", label = h3("Put Your Code Here")),
            actionButton("actionUF", label = "Check"),
            tableOutput('UF'),
            hr()
          ),
          tabPanel(
            "Task G",
            verbatimTextOutput("valueG"),
            fileInput("fileG", label = h3("Put Your Code Here")),
            actionButton("actionUG", label = "Check"),
            tableOutput('UG'),
            hr()
          ),
          tabPanel(
            "Task Z",
            verbatimTextOutput("valueZ"),
            fileInput("fileZ", label = h3("Put Your Code Here")),
            actionButton("actionUZ", label = "Check"),
            tableOutput('UZ'),
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
            fileInput("testsA", label = "Tests here"),
            fileInput("AA", label = "Answers here"),
            actionButton("actionAA", label = "Add description and tests"),
            tableOutput('TA'),
            hr()
          ),
        ),
        tabPanel(
          "Task B",
          sidebarPanel(
            fileInput("descriptionB", label = "Description here"),
            fileInput("testsB", label = "Tests here"),
            fileInput("AB", label = "Answers here"),
            actionButton("actionAB", label = "Add description and tests"),
            tableOutput('TB'),
            hr()
          ),
        ),
        tabPanel(
          "Task C",
          sidebarPanel(
            fileInput("descriptionC", label = "Description here"),
            fileInput("testsC", label = "Tests here"),
            fileInput("AC", label = "Answers here"),
            actionButton("actionAC", label = "Add description and tests"),
            tableOutput('TC'),
            hr()
          ),
        ),
        tabPanel(
          "Task D",
          sidebarPanel(
            fileInput("descriptionD", label = "Description here"),
            fileInput("testsD", label = "Tests here"),
            fileInput("AD", label = "Answers here"),
            actionButton("actionAD", label = "Add description and tests"),
            tableOutput('TD'),
            hr()
          ),
        ),
        tabPanel(
          "Task E",
          sidebarPanel(
            fileInput("descriptionE", label = "Description here"),
            fileInput("testsE", label = "Tests here"),
            fileInput("AE", label = "Answers here"),
            actionButton("actionAE", label = "Add description and tests"),
            tableOutput('TE'),
            hr()
          ),
        ),
        tabPanel(
          "Task F",
          sidebarPanel(
            fileInput("descriptionF", label = "Description here"),
            fileInput("testsF", label = "Tests here"),
            fileInput("AF", label = "Answers here"),
            actionButton("actionAF", label = "Add description and tests"),
            tableOutput('TF'),
            hr()
          ),
        ),
        tabPanel(
          "Task G",
          sidebarPanel(
            fileInput("descriptionG", label = "Description here"),
            fileInput("testsG", label = "Tests here"),
            fileInput("AG", label = "Answers here"),
            actionButton("actionAG", label = "Add description and tests"),
            tableOutput('TG'),
            hr()
          ),
        ),
        tabPanel(
          "Task Z",
          sidebarPanel(
            fileInput("descriptionZ", label = "Description here"),
            fileInput("testsZ", label = "Tests here"),
            fileInput("AZ", label = "Answers here"),
            actionButton("actionAZ", label = "Add description and tests"),
            tableOutput('TZ'),
            hr()
          ),
        )
      )
    )
  )
)
