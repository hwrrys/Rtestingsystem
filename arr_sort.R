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
spl <- function(a) {
  s = ''
  res = list()
  for (i in 1:nchar(a)){
    if (substring(a, i, i) == ',') {
      res = c(res, list(s))
      s = ''
    } else {
      s = paste0(s, substring(a, i, i))
    }
  }
  res = c(res, list(s))
  res[[2]] = strtoi(res[[2]])
  res[[3]] = strtoi(res[[3]])
  return(res)
}
ms <- function(nam,su,st,wb) {
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
      so[[length(so)]][[2]] = so[[length(so)]][[2]]+su
      so[[length(so)]][[3]] = so[[length(so)]][[3]]+st
      if (wb != 0) {
        so[[length(so)]][[3+wb]] = '+'
      }
    }
  }
  so = arr_sort(so)
  conn = file("Table_of_results.txt", "w", encoding = 'UTF-8')
  writeLines("Name,Summ,Fine,A,B,C,D,E,F,G,Z",conn, sep = "\n")
  for (i in 1:(length(so))) {
    print(so[[i]])
    for (j in 1:(length(so[[i]]))) {
      print(so[[i]][[j]])
      if (j != length(so[[i]])) {
        writeLines(paste0(so[[i]][[j]],''),conn, sep = ",")
      } else {
        writeLines(paste0(so[[i]][[j]],''),conn, sep = "\n")
      }
    }
  }
  close(conn)
}
ms('greengolddog',0,0,0)
read.csv("Table_of_results.txt", sep = ",", encoding = 'UTF-8')

