

Dim vbsCardioRVARapp, exeFolder, shcCardioRVARapp
Set vbsCardioRVARapp = WScript.CreateObject("WScript.Shell")
exeFolder = vbsCardioRVARapp.CurrentDirectory
vbsFile = exeFolder & "\exe.vbs"
shcFile = exeFolder & "\CardioRVAR.lnk"
objIcon = exeFolder & "\ICON.ico"
Set shcCardioRVARapp = vbsCardioRVARapp.CreateShortcut(shcFile)
shcCardioRVARapp.TargetPath = vbsFile
shcCardioRVARapp.WindowStyle = 1
shcCardioRVARapp.IconLocation = objIcon
shcCardioRVARapp.Description = "CardioRVAR"
shcCardioRVARapp.Save