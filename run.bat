timeout /t 5
taskkill /im python 1.txt  0<in.txt 1>out.txt 2>ex.txt /f 
taskkill /im cmd.exe /f