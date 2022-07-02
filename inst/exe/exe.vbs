' This code was based from the folowing reference:
' Sellors M. Turn a shiny application into a tablet or desktop app [Internet]. R-bloggers. 2020 [cited 2 July 2022]. 
' Available in: https://www.r-bloggers.com/2020/10/turn-a-shiny-application-into-a-tablet-or-desktop-app/

' C:/Users/ServNFS/Documents/R/win-library/4.1/CardioRVARapp/exe/

Dim objShell, strCurFold
Set objShell = WScript.CreateObject("WScript.Shell")
strCurFold = objShell.CurrentDirectory
objShell.Run(strCurFold & "\exe.bat"), 0, True