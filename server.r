function(input, output) {
  Log <- eventReactive(input$Login, {
    if (input$login == "Admin"){
      if (input$password == "awdrmki9nj") {
        return("Admin")
      }
    } else {
      if (input$login == "reg") {
        return("reg")
      } else {
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
              return("User")
              break
            }
          }
        }
        close(us)
      }
    }
  }, ignoreNULL = FALSE)
  logi <- eventReactive(input$Login, {
    return(input$login)
  }, ignoreNULL = FALSE)
  output$ui <- renderUI({
    if (is.null(input$password))
      return()
    switch(Log(),
           "reg" = mainPanel(
             textInput("plr", label = "enter login"),
             textInput("ppr", label = "enter password"),
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
                 actionButton("actionUA", label = "Check"),
                 tableOutput('UUA'),
                 tableOutput("UA"),
                 hr()
               ),
               tabPanel(
                 "Task B",
                 verbatimTextOutput("valueB"),
                 fileInput("fileB", label = h3("Put Your Code Here")),
                 actionButton("actionUB", label = "Check"),
                 tableOutput('UUB'),
                 tableOutput("UB"),
                 hr()
               ),
               tabPanel(
                 "Task C",
                 verbatimTextOutput("valueC"),
                 fileInput("fileC", label = h3("Put Your Code Here")),
                 actionButton("actionUC", label = "Check"),
                 tableOutput('UUC'),
                 tableOutput("UC"),
                 hr()
               ),
               tabPanel(
                 "Task D",
                 verbatimTextOutput("valueD"),
                 fileInput("fileD", label = h3("Put Your Code Here")),
                 actionButton("actionUD", label = "Check"),
                 tableOutput('UUD'),
                 tableOutput("UD"),
                 hr()
               ),
               tabPanel(
                 "Task E",
                 verbatimTextOutput("valueE"),
                 fileInput("fileE", label = h3("Put Your Code Here")),
                 actionButton("actionUE", label = "Check"),
                 tableOutput('UUE'),
                 tableOutput("UE"),
                 hr()
               ),
               tabPanel(
                 "Task F",
                 verbatimTextOutput("valueF"),
                 fileInput("fileF", label = h3("Put Your Code Here")),
                 actionButton("actionUF", label = "Check"),
                 tableOutput('UUF'),
                 tableOutput("UF"),
                 hr()
               ),
               tabPanel(
                 "Task G",
                 verbatimTextOutput("valueG"),
                 fileInput("fileG", label = h3("Put Your Code Here")),
                 actionButton("actionUG", label = "Check"),
                 tableOutput('UUG'),
                 tableOutput("UG"),
                 hr()
               ),
               tabPanel(
                 "Task Z",
                 verbatimTextOutput("valueZ"),
                 fileInput("fileZ", label = h3("Put Your Code Here")),
                 actionButton("actionUZ", label = "Check"),
                 tableOutput('UUZ'),
                 tableOutput("UZ"),
                 hr()
               )
             ),
           )),
           "Admin" =tabsetPanel(
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
        return("This user been registred")
        c = 1
        break
      }
    }
    if (c == 0) {
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
        FA = file(paste0(input$plr, "A.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "B.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "C.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "D.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "E.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "F.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "G.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
        FA = file(paste0(input$plr, "Z.txt"), "w", encoding = 'UTF-8')
        writeLines("Pogramminglanguage, Result, Test",con, sep = "\n")
        close(FA)
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
    ra = file("AnsA.txt", "r", encoding = 'UTF-8')
    if (input$programminglanguage == 2) {
      conn = file("in.txt", "w", encoding = 'UTF-8')
      con = file("TestsA.txt", "r", encoding = 'UTF-8')
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
            tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
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
                tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
                writeLines("Python, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("Python, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
      }
    } else {
      shell.exec("co++.bat")
      Sys.sleep(10)
      ex = file("ex.txt", "r", encoding = 'UTF-8')
      llll = readLines(ex, n = 1)
      if (length(llll) != 0) {
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, CE", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
        v = 1
      }
      close(ex)
      conn = file("in.txt", "w", encoding = 'UTF-8')
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
          shell.exec("c++.bat")
          Sys.sleep(2)
          ya = file("out.txt", "r", encoding = 'UTF-8')
          while(TRUE) {
            line2 = readLines(ya, n = 1)
            l = readLines(ra, n = 1)
            if ( length(line2) == 0 ) {
              if (l != ".") {
                tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
                writeLines("C++, PE", tt, sep = ",")
                writeLines(paste0('',t), tt, sep = "\n")
                close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
        tt = file(paste0(name, "A.txt"), 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "B.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "C.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              writeLines("Python, PD", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "D.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              writeLines("Python, PD", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "E.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              writeLines("Python, PD", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "F.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              writeLines("Python, PD", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "G.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              writeLines("Python, PD", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("Python, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
              v = 1
              break
            }
            if (line2 != l) {
              tt = file(paste0(name, "Z.txt"), 'a', encoding = 'UTF-8')
              writeLines("C++, WA", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
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
      print(line)
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
      print(line)
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
      print(line)
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
      print(line)
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
      print(line)
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
      print(line)
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
      print(line)
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
      print(line)
    }
    close(con)
  })
}
