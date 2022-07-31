
.onLoad <- function(libname, pkgname){
  user <- Sys.getenv("USERPROFILE")
  shortcDesk <- paste(user, "\\Desktop\\CardioRVAR.lnk", sep = "")
  if(!file.exists(shortcDesk)){
    bool_shc <- readline("Do you want to create a shortcut? [y/n]: ")
    if(bool_shc == "y"){
      CreateCardioRVARShc()
    }
  }
}





