con = file("C:/Users/losef/Desktop/Rtestingsystem/ab.bat", "w", encoding = 'UTF-8')
writeLines("timeout 5", con, sep = "\n")
close(con)
#file.rename("C:/Users/losef/Desktop/Rtestingsystem/ab.txt", "C:/Users/losef/Desktop/Rtestingsystem/ab.bat")
shell.exec("C:/Users/losef/Desktop/Rtestingsystem/ab.bat")


