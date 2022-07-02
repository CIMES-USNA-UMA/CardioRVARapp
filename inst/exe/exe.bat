:: This code was based from the folowing reference:
:: Sellors M. Turn a shiny application into a tablet or desktop app [Internet]. R-bloggers. 2020 [cited 2 July 2022]. 
:: Available in: https://www.r-bloggers.com/2020/10/turn-a-shiny-application-into-a-tablet-or-desktop-app/

@ECHO OFF
"C:\Program Files\R\R-4.1.1\bin\Rscript.exe" -e "shiny::runApp(system.file("app", package = "CardioRVARapp"), 
        launch.browser = TRUE)"
