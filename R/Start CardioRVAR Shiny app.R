

#' Start CardioRVARapp
#'
#' Launches CardioRVARapp in a browser
#'         
#' @author Alvaro Chao-Ecija, Marc Stefan Dawid-Milner
#'         
#'
#' @export
#'
#' @examples
#' StartCardioRVARapp()
#' 
StartCardioRVARapp <- function(){
  shiny::runApp(system.file("app", package = "CardioRVARapp"), launch.browser = TRUE)
}


