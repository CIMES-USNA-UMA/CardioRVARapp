' This code was based from the folowing reference:
' Sellors M. Turn a shiny application into a tablet or desktop app [Internet]. R-bloggers. 2020 [cited 2 July 2022]. 
' Available in: https://www.r-bloggers.com/2020/10/turn-a-shiny-application-into-a-tablet-or-desktop-app/


Dim exeCardioRVARapp, exeFolder
Set exeCardioRVARapp = WScript.CreateObject("WScript.Shell")
exeFolder = exeCardioRVARapp.CurrentDirectory
exeCardioRVARapp.Run(exeFolder & "\exe.bat"), 0, True