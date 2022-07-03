' This code is based on the following reference:
'
' VBScript - Creating a Shortcut [Internet]. Vbsedit.com. 2022 [cited 3 July 2022]. Available from: ' https://www.vbsedit.com/html/e6ac108b-15f6-4a54-891f-589e8b687ace.asp

Set mScript = WScript.CreateObject("WScript.Shell") 
shcFile = "%CardioRVAR_app_shortcut%" 
Set shcCardioRVARapp = mScript.CreateShortcut(shcFile) 
vbsCardioRVARapp = "%CardioRVAR_app_vbScript_for_shortcut%" 
iconCardioRVARapp = "%CardioRVAR_app_icon_for_shortcut%" 
shcCardioRVARapp.TargetPath = vbsCardioRVARapp 
shcCardioRVARapp.IconLocation = iconCardioRVARapp
shcCardioRVARapp.Save 


