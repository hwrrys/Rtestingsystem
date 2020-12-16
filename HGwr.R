library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)


Sys.setlocale("LC_ALL", "Russian")
aa <<- 0
b <<- 0
rez <<- ''
nor <<- 0


ui <-
    dashboardPage(skin ="green",
      dashboardHeader(tags$li(class = "dropdown",actionButton("Login", label = "Login/Logout", icon("arrow-left"),style = "color: #fff; background-color: #00a65a; border-color: #00a65a")),
                     (tags$li(class = "dropdown",actionButton("registery", label = "Register",icon("pencil"), style = "color: #fff; background-color: #00a65a; border-color: #00a65a"))),
      title = "HGwr",
      titleWidth = 230),
      dashboardSidebar(
  
        sidebarMenu(
          menuItem("Tasks", tabName = "dashboard", icon = icon("tasks")),
          menuItem("Scoreboard", tabName = "scoreboard", icon = icon("signal")),
          menuItem("Settings", tabName = "settings", icon = icon("cog"))
        )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                mainPanel(
                  theme = shinytheme("paper"),
                  
                ),
                uiOutput("ui")
        ),
        
        tabItem(tabName = "scoreboard",
                dataTableOutput("tab_res"),
                actionButton("TaU", label = "Update Table", icon("sync")),
                hr()
        ),
        tabItem(tabName = "settings",
        )
      ),
    )
  )

server <- function(input, output) {
  arr_sort <- function(c) {
    i = length(c)-1
    while(i > 0) {
      for (j in 1:i) {
        if (c[[j]][[2]] < c[[j+1]][[2]]){
          x = c[j]
          c[j] = c[j+1]
          c[j+1] = x
        } else if (c[[j]][[2]] == c[[j+1]][[2]]){
          if (c[[j]][[3]] > c[[j+1]][[3]]){
            x = c[j]
            c[j] = c[j+1]
            c[j+1] = x
          }
        }
      }
      i = i - 1
    }
    return(c)
  }
  spl <- function(aa) {
    s = ''
    res = list()
    for (i in 1:nchar(aa)){
      if (substring(aa, i, i) == ',') {
        res = c(res, list(s))
        s = ''
      } else {
        s = paste0(s, substring(aa, i, i))
      }
    }
    res = c(res, list(s))
    res[[2]] = strtoi(res[[2]])
    res[[3]] = strtoi(res[[3]])
    return(res)
  }
  ms <- function(nam,ss,wb) {
    con = file("Table_of_results.txt", "r", encoding = 'UTF-8')
    line = readLines(con, n = 1)
    so = list()
    while(TRUE) {
      line = readLines(con, n = 1)
      if (length(line) == 0){
        close(con)
        break
      }
      so[[length(so)+1]] = spl(line)
      if (so[[length(so)]][[1]] == nam) {
        if (ss == 1) {
          so[[length(so)]][[2]] = so[[length(so)]][[2]]+1
          if (nchar(so[[length(so)]][[3+wb]]) == 1) {
            so[[length(so)]][[3+wb]] = '+'
          } else {
            so[[length(so)]][[3]] = so[[length(so)]][[3]]+strtoi(substring(so[[length(so)]][[3+wb]],2,nchar(so[[length(so)]][[3+wb]])))
            so[[length(so)]][[3+wb]] = paste0('+',strtoi(substring(so[[length(so)]][[3+wb]],2,nchar(so[[length(so)]][[3+wb]]))))
          }
        } else {
          if (nchar(so[[length(so)]][[3+wb]]) == 1) {
            print(1)
            so[[length(so)]][[3+wb]] = paste0('-',1)
          } else {
            print(2)
            so[[length(so)]][[3+wb]] = paste0('-',strtoi(substring(so[[length(so)]][[3+wb]],2,nchar(so[[length(so)]][[3+wb]])))+1)
          }
        }
      }
    }
    so = arr_sort(so)
    conn = file("Table_of_results.txt", "w", encoding = 'UTF-8')
    writeLines("Name,Summ,Fine,A,B,C,D,E,F,G,Z",conn, sep = "\n")
    for (i in 1:(length(so))) {
      for (j in 1:(length(so[[i]]))) {
        if (j != length(so[[i]])) {
          writeLines(paste0(so[[i]][[j]],''),conn, sep = ",")
        } else {
          writeLines(paste0(so[[i]][[j]],''),conn, sep = "\n")
        }
      }
    }
    close(conn)
  }
  ud<- function(za) {
    con = file("Table_of_results.txt", "r", encoding = 'UTF-8')
    line = readLines(con, n = 1)
    so = list()
    while(TRUE) {
      line = readLines(con, n = 1)
      if (length(line) == 0){
        close(con)
        break
      }
      so[[length(so)+1]] = spl(line)
      if (substring(so[[length(so)]][[3+za]],1,1) == '+'){
        so[[length(so)]][[2]] = so[[length(so)]][[2]] - 1
        if (nchar(so[[length(so)]][[3+za]]) != 1) {
          so[[length(so)]][[3]] = so[[length(so)]][[3]] - strtoi(substring(so[[length(so)]][[3+za]],2,nchar(so[[length(so)]][[3+za]])))
        }
      }
      so[[length(so)]][[3+za]] = '-'
    }
    so = arr_sort(so)
    conn = file("Table_of_results.txt", "w", encoding = 'UTF-8')
    writeLines("Name,Summ,Fine,A,B,C,D,E,F,G,Z",conn, sep = "\n")
    for (i in 1:(length(so))) {
      for (j in 1:(length(so[[i]]))) {
        if (j != length(so[[i]])) {
          writeLines(paste0(so[[i]][[j]],''),conn, sep = ",")
        } else {
          writeLines(paste0(so[[i]][[j]],''),conn, sep = "\n")
        }
      }
    }
    close(conn)
  }
  Logg <- eventReactive(input$registery, {
    if (input$registery != 0){
      assign("rez", "reg", envir = .GlobalEnv)
      assign("aa", 1, envir = .GlobalEnv)
    }
  }, ignoreNULL = FALSE)
  Log <- eventReactive(input$Login, {
    print(aa)
    if (aa == 1){
      assign("rez", "", envir = .GlobalEnv)
      assign("aa", 0, envir = .GlobalEnv)
      return("No")
    }
    if (input$Login == 0){
      return("No")
    } else {
      if (input$login == "Admin"){
        if (input$password == "awdrmki9nj") {
          assign("aa", 1, envir = .GlobalEnv)
          return("Admin")
        }
      }
      us = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        logins = readLines(us, n = 1)
        if (length(logins) == 0) {
          return("No")
          break
        }
        passwords = readLines(us, n = 1)
        if (logins == input$login) {
          if (passwords == input$password) {
            assign("aa", 1, envir = .GlobalEnv)
            return("User")
            break
          }
        }
      }
      close(us)
    }
  }, ignoreNULL = FALSE)
  logi <- eventReactive(input$Login, {
    return(input$login)
  }, ignoreNULL = FALSE)
  output$ui <- renderUI({
    Logg()
    switch(paste0(Log(), rez),
           "No" = mainPanel(
             textInput("login","Login:"),
             textInput("password","Password:"),
           ),
           "Noreg" = mainPanel(
             textInput("plr", label = "Enter login"),
             textInput("ppr", label = "Enter password"),
             actionButton("registerb", label = "Register"),
             verbatimTextOutput("regt")
           ),
           "Userreg" = mainPanel(
             textInput("plr", label = "Enter login"),
             textInput("ppr", label = "Enter password"),
             actionButton("registerb", label = "Register"),
             verbatimTextOutput("regt")
           ),
           "Adminreg" = mainPanel(
             textInput("plr", label = "Enter login"),
             textInput("ppr", label = "Enter password"),
             actionButton("registerb", label = "Register"),
             verbatimTextOutput("regt")
           ),
           "User" = mainPanel(sidebarPanel(
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
                 actionButton("actionUA", label = "Check", icon("paper-plane")),
                 tableOutput('UUA'),
                 tableOutput("UA"),
                 hr()
               ),
               tabPanel(
                 "Task B",
                 verbatimTextOutput("valueB"),
                 fileInput("fileB", label = h3("Put Your Code Here")),
                 actionButton("actionUB", label = "Check", icon("paper-plane")),
                 tableOutput('UUB'),
                 tableOutput("UB"),
                 hr()
               ),
               tabPanel(
                 "Task C",
                 verbatimTextOutput("valueC"),
                 fileInput("fileC", label = h3("Put Your Code Here")),
                 actionButton("actionUC", label = "Check", icon("paper-plane")),
                 tableOutput('UUC'),
                 tableOutput("UC"),
                 hr()
               ),
               tabPanel(
                 "Task D",
                 verbatimTextOutput("valueD"),
                 fileInput("fileD", label = h3("Put Your Code Here")),
                 actionButton("actionUD", label = "Check", icon("paper-plane")),
                 tableOutput('UUD'),
                 tableOutput("UD"),
                 hr()
               ),
               tabPanel(
                 "Task E",
                 verbatimTextOutput("valueE"),
                 fileInput("fileE", label = h3("Put Your Code Here")),
                 actionButton("actionUE", label = "Check", icon("paper-plane")),
                 tableOutput('UUE'),
                 tableOutput("UE"),
                 hr()
               ),
               tabPanel(
                 "Task F",
                 verbatimTextOutput("valueF"),
                 fileInput("fileF", label = h3("Put Your Code Here")),
                 actionButton("actionUF", label = "Check", icon("paper-plane")),
                 tableOutput('UUF'),
                 tableOutput("UF"),
                 hr()
               ),
               tabPanel(
                 "Task G",
                 verbatimTextOutput("valueG"),
                 fileInput("fileG", label = h3("Put Your Code Here")),
                 actionButton("actionUG", label = "Check", icon("paper-plane")),
                 tableOutput('UUG'),
                 tableOutput("UG"),
                 hr()
               ),
               tabPanel(
                 "Task Z",
                 verbatimTextOutput("valueZ"),
                 fileInput("fileZ", label = h3("Put Your Code Here")),
                 actionButton("actionUZ", label = "Check", icon("paper-plane")),
                 tableOutput('UUZ'),
                 tableOutput("UZ"),
                 hr()
               )
             ),
           )),
           "Admin" = mainPanel(
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Task A",
                 sidebarPanel(
                   fileInput("descriptionA", label = "Description here"),
                   fileInput("testsA", label = "Tests here"),
                   fileInput("AA", label = "Answers here"),
                   checkboxInput("chA1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAA", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chB1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAB", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chC1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAC", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chD1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAD", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chE1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAE", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chF1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAF", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chG1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAG", label = "Add description and tests", icon("sync")),
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
                   checkboxInput("chZ1", label = "Delete all results of users?", value = TRUE),
                   actionButton("actionAZ", label = "Add description and tests", icon("sync")),
                   tableOutput('TZ'),
                   hr()
                 )
               )
             )
           )
    )
  })
  reeeg <- eventReactive(input$registerb, {
    con = file("Users.txt", "r", encoding = 'UTF-8')
    c = 0
    while (TRUE) {
      line = readLines(con, n = 1)
      if (length(line) == 0) {
        break
      }
      ll = readLines(con, n = 1)
      if (line == input$plr) {
        close(con)
        return("This user been registred")
        c = 1
        break
      }
    }
    close(con)
    if (c == 0) {
      if (input$plr == 'Admin') {
        c = 1
        return("This user been registred")
      }
      if (input$plr == 'Task') {
        c = 1
        return("This user been registred")
      }
      if (input$plr == 'Tests') {
        c = 1
        return("This user been registred")
      }
      if (input$plr == 'Ans') {
        c = 1
        return("This user been registred")
      }
      if (input$plr == '') {
        c = 1
        return("Enter a non-empty login")
      }
      if (input$ppr == '') {
        c = 1
        return("Enter a non-empty password")
      }
      if (c == 0) {
        con = file("Users.txt", "a", encoding = 'UTF-8')
        writeLines(input$plr,con, sep = "\n")
        writeLines(input$ppr,con, sep = "\n")
        close(con)
        con = file("Table_of_results.txt", "a", encoding = 'UTF-8')
        writeLines(paste0(input$plr, ",0,0,-,-,-,-,-,-,-,-"),con, sep = "\n")
        close(con)
        FA = file(paste0(input$plr, "A.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "B.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "C.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "D.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "E.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "F.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "G.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "Z.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        bp = file(paste0(input$plr, "p.bat"), "w", encoding = 'UTF-8')
        writeLines(paste0("python ", paste0(input$plr, paste0("1.txt > ", paste0(input$plr, paste0("out.txt < ", paste0(input$plr, paste0("in.txt 2> ", paste0(input$plr, "ex.txt")))))))), bp, sep = "\n")
        close(bp)
        tx = file(paste0(input$plr, "1.txt"), "w", encoding = 'UTF-8')
        close(tx)
        tx = file(paste0(input$plr, "out.txt"), "w", encoding = 'UTF-8')
        close(tx)
        tx = file(paste0(input$plr, "in.txt"), "w", encoding = 'UTF-8')
        close(tx)
        tx = file(paste0(input$plr, "ex.txt"), "w", encoding = 'UTF-8')
        close(tx)
        bp = file(paste0(input$plr, "c++.bat"), "w", encoding = 'UTF-8')
        writeLines("set PATH=mingw64\\bin;%PATH%",bp, sep = "\n")
        writeLines("rem echo %PATH%",bp, sep = "\n")
        writeLines('rem cd "mingw64\\bin"\\',bp, sep = "\n")
        writeLines(paste0(input$plr, paste0("a.exe > ", paste0(input$plr, paste0("out.txt < ", paste0(input$plr, paste0("in.txt 2> ", paste0(input$plr, "ex.txt"))))))), bp, sep = "\n")
        close(bp)
        bp = file(paste0(input$plr, "co++.bat"), "w", encoding = 'UTF-8')
        writeLines("set PATH=mingw64\\bin;%PATH%",bp, sep = "\n")
        writeLines("rem echo %PATH%",bp, sep = "\n")
        writeLines('rem cd "mingw64\\bin"\\',bp, sep = "\n")
        writeLines(paste0("g++ -o ", paste0(input$plr, paste0("a.exe ", paste0(input$plr, paste0("1c.cpp 2> ", paste0(input$plr, "ex.txt")))))), bp, sep = "\n")
        close(bp)
        tx = file(paste0(input$plr, "1c.cpp"), "w", encoding = 'UTF-8')
        close(tx)
        assign("rez", "", envir = .GlobalEnv)
        return(paste0("Registeation was successful and you registerd with name ", input$plr))
      }
    }
  }, ignoreNULL = TRUE)
  output$regt <- renderPrint({
    reeeg()
  })
  AU <- eventReactive(input$actionUA, {
    name = logi()
    con = file(input$fileA$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file(paste0(name, "1.txt"), "w", encoding = 'UTF-8')
    } else {
      conn = file(paste0(name, "1c.cpp"), "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsA.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file(paste0(name, "in.txt"), "w", encoding = 'UTF-8')
      con = file("TestsA.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec(paste0(name, "p.bat"))
          Sys.sleep(2)
          ex = file(paste0(name, "ex.txt"), "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            if (t == 1) {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, CE", tt, sep = ",")
              writeLines("-", tt, sep = "\n")
              close(tt)
            } else {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, RT", tt, sep = ",")
              writeLines(paste0(t, ""), tt, sep = "\n")
              close(tt)
              ms(name,0,1)
            }
            v = 1
            break
          }
          close(ex)
          ya = file(paste0(name, "out.txt"), "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,1)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,1)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,1)
              v = 1
              break
            }
          }
          close(ya)
          conn = file(paste0(name, "in.txt"), "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,1)
      }
    } else {
      shell.exec(paste0(name, "co++.bat"))
      Sys.sleep(10)
      ex = file(paste0(name, "ex.txt"), "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file(paste0(name, "in.txt"), "w", encoding = 'UTF-8')
      con = file("TestsA.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec(paste0(name, "c++.bat"))
          Sys.sleep(2)
          ya = file(paste0(name, "out.txt"), "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,1)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,1)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,1)
              v = 1
              break
            }
          }
          close(ya)
          conn = file(paste0(name, "in.txt"), "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,1)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "A.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UA <- renderTable({
    if (input$actionUA == 0) {
      read.csv(paste0(logi(), "A.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUA <- renderTable({
    AU()
  })
  BU <- eventReactive(input$actionUB, {
    name = logi()
    con = file(input$fileB$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsB.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsB.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,2)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,2)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,2)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,2)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsB.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,2)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,2)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,2)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,2)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "B.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UB <- renderTable({
    if (input$actionUB == 0) {
      read.csv(paste0(logi(), "B.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUB <- renderTable({
    BU()
  })
  CU <- eventReactive(input$actionUC, {
    name = logi()
    con = file(input$fileC$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsC.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsC.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,3)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,3)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,3)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,3)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsC.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,3)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,3)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,3)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,3)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "C.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UC <- renderTable({
    if (input$actionUC == 0) {
      read.csv(paste0(logi(), "C.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUC <- renderTable({
    CU()
  })
  DU <- eventReactive(input$actionUD, {
    name = logi()
    con = file(input$fileD$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsD.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsD.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,4)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,4)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,4)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,4)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsD.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,4)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,4)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,4)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,4)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "D.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UD <- renderTable({
    if (input$actionUD == 0) {
      read.csv(paste0(logi(), "D.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUD <- renderTable({
    DU()
  })
  EU <- eventReactive(input$actionUE, {
    name = logi
    con = file(input$fileE$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsE.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsE.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,5)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,5)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,5)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,5)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsE.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,5)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,5)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,5)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,5)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "E.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UE <- renderTable({
    if (input$actionUE == 0) {
      read.csv(paste0(logi(), "E.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUE <- renderTable({
    EU()
  })
  FU <- eventReactive(input$actionUF, {
    name = logi()
    con = file(input$fileF$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsF.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsF.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,6)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,6)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,6)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,6)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsF.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,6)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,6)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,6)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,6)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "F.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UF <- renderTable({
    if (input$actionUF == 0) {
      read.csv(paste0(logi(), "F.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUF <- renderTable({
    FU()
  })
  GU <- eventReactive(input$actionUG, {
    name = logi()
    con = file(input$fileG$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsG.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsG.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,7)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,7)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,7)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,7)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsG.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,7)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,7)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,7)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,7)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "G.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UG <- renderTable({
    if (input$actionUG == 0) {
      read.csv(paste0(logi(), "G.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUG <- renderTable({
    GU()
  })
  ZU <- eventReactive(input$actionUZ, {
    name = logi()
    con = file(input$fileZ$name, "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("1.txt", "w", encoding = 'UTF-8')
    } else {
      conn = file("1c.cpp", "w", encoding = 'UTF-8')
    }
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    v = 0
    t = 0
    ra = file("AnsZ.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsZ.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("p.bat")
          Sys.sleep(2)
          ex = file("ex.txt", "r", encoding = 'UTF-8')
          q = readLines(ex, n = 1)
          if (length(q) != 0){
            tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
            writeLines("Python, CE", tt, sep = ",")
            writeLines("-", tt, sep = "\n")
            close(tt)
            v = 1
            break
          }
          close(ex)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,8)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,8)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,8)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
          if (v == 1) {
            break
          }
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,8)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsZ.txt", "r", encoding = 'UTF-8')
      while ( TRUE ) {
        if (v == 1) {
          break
        }
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        if (line == ".") {
          t = t + 1
          close(conn)
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
                ms(name,0,8)
                v = 1
                while (TRUE) {
                  lo = readLines(ra, n = 1)
                  if (lo == "."){
                    break
                  }
                }
              }
              break
            }
            if (l == '.') {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,8)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              ms(name,0,8)
              v = 1
              break
            }
          }
          close(ya)
          conn = file("in.txt", "w", encoding = 'UTF-8')
        } else {
          writeLines(line,conn, sep = "\n")
        }
      }
      if (v == 0) {
        tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        ms(name,1,8)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv(paste0(name, "Z.txt"), sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$UZ <- renderTable({
    if (input$actionUZ == 0) {
      read.csv(paste0(logi(), "Z.txt"), sep = ",", encoding = 'UTF-8')
    }
  })
  output$UUZ <- renderTable({
    ZU()
  })
  A <- eventReactive(input$actionAA, {
    con = file(input$descriptionA$name, "r", encoding = 'UTF-8')
    conn = file("TaskA.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsA$name, "r", encoding = 'UTF-8')
    conn = file("TestsA.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AA$name, "r", encoding = 'UTF-8')
    conn = file("AnsA.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chA1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "A.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(1)
    }
    read.csv(input$descriptionA$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TA <- renderTable({
    A()
  })
  B <- eventReactive(input$actionAB, {
    con = file(input$descriptionB$name, "r", encoding = 'UTF-8')
    conn = file("TaskB.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsB$name, "r", encoding = 'UTF-8')
    conn = file("TestsB.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AB$name, "r", encoding = 'UTF-8')
    conn = file("AnsB.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chB1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "B.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(2)
    }
    read.csv(input$descriptionB$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TB <- renderTable({
    B()
  })
  C <- eventReactive(input$actionAC, {
    con = file(input$descriptionC$name, "r", encoding = 'UTF-8')
    conn = file("TaskC.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsC$name, "r", encoding = 'UTF-8')
    conn = file("TestsC.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AC$name, "r", encoding = 'UTF-8')
    conn = file("AnsC.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chC1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "C.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(3)
    }
    read.csv(input$descriptionC$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TC <- renderTable({
    C()
  })
  D <- eventReactive(input$actionAD, {
    con = file(input$descriptionD$name, "r", encoding = 'UTF-8')
    conn = file("TaskD.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsD$name, "r", encoding = 'UTF-8')
    conn = file("TestsD.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AD$name, "r", encoding = 'UTF-8')
    conn = file("AnsD.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chD1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "D.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(4)
    }
    read.csv(input$descriptionD$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TD <- renderTable({
    D()
  })
  E <- eventReactive(input$actionAE, {
    con = file(input$descriptionE$name, "r", encoding = 'UTF-8')
    conn = file("TaskE.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsE$name, "r", encoding = 'UTF-8')
    conn = file("TestsE.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AE$name, "r", encoding = 'UTF-8')
    conn = file("AnsE.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chE1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "E.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(5)
    }
    read.csv(input$descriptionE$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TE <- renderTable({
    E()
  })
  ff <- eventReactive(input$actionAF, {
    con = file(input$descriptionF$name, "r", encoding = 'UTF-8')
    conn = file("TaskF.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsF$name, "r", encoding = 'UTF-8')
    conn = file("TestsF.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AF$name, "r", encoding = 'UTF-8')
    conn = file("AnsF.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chF1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "F.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(6)
    }
    read.csv(input$descriptionF$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TF <- renderTable({
    ff()
  })
  G <- eventReactive(input$actionAG, {
    con = file(input$descriptionG$name, "r", encoding = 'UTF-8')
    conn = file("TaskG.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsG$name, "r", encoding = 'UTF-8')
    conn = file("TestsG.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AG$name, "r", encoding = 'UTF-8')
    conn = file("AnsG.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chG1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "G.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(7)
    }
    read.csv(input$descriptionG$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TG <- renderTable({
    G()
  })
  Z <- eventReactive(input$actionAZ, {
    con = file(input$descriptionZ$name, "r", encoding = 'UTF-8')
    conn = file("TaskZ.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$testsZ$name, "r", encoding = 'UTF-8')
    conn = file("TestsZ.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    con = file(input$AZ$name, "r", encoding = 'UTF-8')
    conn = file("AnsZ.txt", "w", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      writeLines(line,conn, sep = "\n")
    }
    close(con)
    close(conn)
    if (input$chZ1 == TRUE) {
      use = file("Users.txt", "r", encoding = 'UTF-8')
      while (TRUE) {
        line = readLines(use, n = 1)
        if (length(line) == 0) {
          break
        }
        musor = readLines(use, n = 1)
        conn = file(paste0(line, "Z.txt"), "w", encoding = 'UTF-8')
        writeLines("Programminglanguage, Result, Test",conn, sep = "\n")
        close(conn)
      }
      close(use)
      ud(8)
    }
    read.csv(input$descriptionZ$name, sep = '`', dec = '~', encoding = 'UTF-8')
  }, ignoreNULL = TRUE)
  output$TZ <- renderTable({
    Z()
  })
  output$valueA <- renderPrint({
    con = file("TaskA.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueB <- renderPrint({
    con = file("TaskB.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueC <- renderPrint({
    con = file("TaskC.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueD <- renderPrint({
    con = file("TaskD.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueE <- renderPrint({
    con = file("TaskE.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueF <- renderPrint({
    con = file("TaskF.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueG <- renderPrint({
    con = file("TaskG.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  output$valueZ <- renderPrint({
    con = file("TaskZ.txt", "r", encoding = 'UTF-8')
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      cat(line)
      cat("\n")
    }
    close(con)
  })
  TabU <- eventReactive(input$TaU, {
    read.csv("Table_of_results.txt", sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = FALSE)
  output$tab_res <- renderDataTable({
    TabU()
  })
}
shinyApp(ui, server)