




CreateCardioRVARShc <- function(){
  user <- Sys.getenv("USERPROFILE")
  shortcDesk <- paste(user, "\\Desktop\\CardioRVAR.lnk", sep = "")
  if(!file.exists(shortcDesk)){
    directory <- getwd()
    pckLocation <- find.package("CardioRVARapp")
    temp_directory <- paste(pckLocation, "/runApp", sep = "")
    setwd(temp_directory)
    shcgen <- paste(pckLocation, "/runApp/createCardioRVARshortcut.vbs", sep = "")
    shell.exec(shQuote(normalizePath(shcgen)))
    setwd(directory)
  }
}