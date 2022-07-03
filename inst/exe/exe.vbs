' This code was based from the folowing reference:
' Sellors M. Turn a shiny application into a tablet or desktop app [Internet]. R-bloggers. 2020 [cited 2 July 2022]. 
' Available in: https://www.r-bloggers.com/2020/10/turn-a-shiny-application-into-a-tablet-or-desktop-app/


Dim CardioRVARapp, mainFolder
Set CardioRVARapp = WScript.CreateObject("WScript.Shell")
mainFolder = "%CarioRVAR_app_directory_folder%"
CardioRVARapp.Run(mainFolder & "\exe.bat"), 0, True