

.onLoad <- function(libname, pkgname){
  user <- Sys.getenv("USERPROFILE")
  shortcDesk <- paste(user, "\\Desktop\\CardioRVAR.lnk", sep = "")
  if(!file.exists(shortcDesk)){
    # Update batch file
    pckLocation <- find.package("CardioRVARapp")
    shortcLoc <-  paste(pckLocation, "/CardioRVAR.lnk", sep = "")
    batch <- paste(pckLocation, "/exe/exe.bat", sep = "")
    batch_code <- readLines(batch)
    Rversion <- getRversion()
    new_str <- paste("R-", Rversion, sep = "")
    new_code <- gsub(pattern = "%R-version-for-CardioRVARapp%", replace = new_str, x = batch_code, 
                     fixed = TRUE)
    writeLines(new_code, con = batch)
    # Update vbScript for batch file
    exevbs <- paste(pckLocation, "/exe/exe.vbs", sep = "")
    vbscode = readLines(exevbs)
    vbscode = gsub("%CarioRVAR_app_directory_folder%", paste(pckLocation, "/exe",
                                                           sep = ""), vbscode,
                   fixed = TRUE)
    writeLines(vbscode, exevbs)
    # Update shortcut generator
    shcgen <- paste(pckLocation, "/shc/shc.vbs", sep = "")
    icon <- paste(pckLocation, "/shc/icon.ico", sep = "")
    shcgen_code <- readLines(shcgen)
    new_shcgen_code = gsub("%CardioRVAR_app_shortcut%", shortcLoc
                           , shcgen_code, fixed = TRUE)
    new_shcgen_code = gsub("%CardioRVAR_app_vbScript_for_shortcut%", exevbs
                           , new_shcgen_code, fixed = TRUE)
    new_shcgen_code = gsub("%CardioRVAR_app_icon_for_shortcut%", icon
                           , new_shcgen_code, fixed = TRUE)
    writeLines(new_shcgen_code, shcgen)
    # Create Shortcut
    shell(shQuote(normalizePath(shcgen)), "cscript", flag = "//nologo")
    # Copy shortcut to desktop
    file.copy(shortcLoc, shortcDesk, overwrite = TRUE)
  }
}





