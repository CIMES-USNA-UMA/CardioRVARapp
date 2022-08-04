' This code is based on the following reference:
'
' VBScript - Creating a Shortcut [Internet]. Vbsedit.com. 2022 [cited 3 July 2022]. Available from: 
' https://www.vbsedit.com/html/e6ac108b-15f6-4a54-891f-589e8b687ace.asp

Set mScript = WScript.CreateObject("WScript.Shell") 
mainFolder = mScript.CurrentDirectory
myDesktop = mScript.SpecialFolders("Desktop")
shcFile = myDesktop & "\CardioRVAR.lnk" 
CardioRVARicon = mainFolder & "\icon.ico"
runCardioRVARapp = mainFolder & "\runCardioRVARapp.vbs"
Set shcCardioRVARapp = mScript.CreateShortcut(shcFile) 
shcCardioRVARapp.TargetPath = runCardioRVARapp
shcCardioRVARapp.IconLocation = CardioRVARicon
shcCardioRVARapp.WorkingDirectory = mainFolder
shcCardioRVARapp.Save 
Set shcCardioRVARapp = Nothing
Set mScript = Nothing
Set mainFolder = Nothing
Set myDesktop = Nothing
Set shcFile = Nothing
Set runCardioRVARapp = Nothing
Set CardioRVARicon = Nothing


