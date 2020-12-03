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
ms <- function(nam,ss,wb) {
  con = file("C:/Users/losef/Desktop/Rtestingsystem/Table_of_results.txt", "r", encoding = 'UTF-8')
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
  conn = file("C:/Users/losef/Desktop/Rtestingsystem/Table_of_results.txt", "w", encoding = 'UTF-8')
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
ms('P',1,2)
read.csv("C:/Users/losef/Desktop/Rtestingsystem/Table_of_results.txt", sep = ",", encoding = 'UTF-8')

