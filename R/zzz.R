

.onLoad <- function(libname, pkgname){
  user <- Sys.getenv("USERPROFILE")
  shortcDesk <- paste(user, "\\Desktop\\CardioRVAR.lnk", sep = "")
  if(!file.exists(shortcDesk)){
    pckLocation <- find.package("CardioRVARapp")
    batch <- paste(pckLocation, "/exe/exe.bat", sep = "")
    batch_code <- readLines(batch)
    Rversion <- getRversion()
    new_str <- paste("R\\R-", Rversion, "\\bin", sep = "")
    new_code <- gsub(pattern = "R\\R-4.1.1\\bin", replace = new_str, x = batch_code, 
                     fixed = TRUE)
    writeLines(new_code, con = batch)
    shcgen <- paste(pckLocation, "/exe/shc.vbs", sep = "")
    shgcen <- gsub(pattern = "/", replacement = "\\", x = shcgen, fixed = TRUE)
    shell(shQuote(normalizePath(shcgen)), "cscript", flag = "//nologo")
    shortcLoc <- paste(pckLocation, "/exe/CardioRVAR.lnk", sep = "")
    file.copy(shortcLoc, shortcDesk)
  }
}
