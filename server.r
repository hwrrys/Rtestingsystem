function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$password))
      return()
    
    switch(input$password,
      "UserPassword" = mainPanel(sidebarPanel(
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
        )),
      "AdminPassword" =tabsetPanel(
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
  
  
  
  
  
  
  AU <- eventReactive(input$actionUA, {
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
            tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
                tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
              tt = file("PA.txt", 'a', encoding = 'UTF-8')
              writeLines("Python, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
        tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
        tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
                tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
              tt = file("PA.txt", 'a', encoding = 'UTF-8')
              writeLines("C++, PE", tt, sep = ",")
              writeLines(paste0('',t), tt, sep = "\n")
              close(tt)
              v = 1
              break
            }
            if (line2 != l) {
              tt = file("PA.txt", 'a', encoding = 'UTF-8')
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
        tt = file("PA.txt", 'a', encoding = 'UTF-8')
        writeLines("C++, OK", tt, sep = ",")
        writeLines("-", tt, sep = "\n")
        close(tt)
      }
    }
    close(con)
    close(conn)
    close(ra)
    read.csv("PA.txt", sep = ",", encoding = 'UTF-8')
  }, ignoreNULL = FALSE)
  output$UA <- renderTable({
    AU()
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
  }, ignoreNULL = FALSE)
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
