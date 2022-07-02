

.onLoad <- function(libname, pkgname){
  user <- Sys.getenv("USERPROFILE")
  shortcDesk <- paste(user, "\\Desktop\\CardioRVAR.lnk", sep = "")
  if(!file.exists(shortcDesk)){
    pckLocation <- find.package("CardioRVARapp")
    shortcLoc <- paste(pckLocation, "/CardioRVAR.lnk", sep = "")
    file.copy(shortcLoc, shortcDesk)
  }
}
