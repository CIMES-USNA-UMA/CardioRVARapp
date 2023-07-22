

#' Start CardioRVARapp
#'
#' Launches CardioRVARapp in a browser
#'         
#' @author Alvaro Chao-Ecija, Marc Stefan Dawid-Milner
#'
#' @import ggplot2
#' @import tools
#'
#' @export
#'
#' @examples
#' \dontrun{
#' StartCardioRVARapp()
#' }
#' 
#' 
StartCardioRVARapp <- function(){
  if (!requirerNamespace("shiny"))
    stop(
      "Package 'shiny' must be installed to access CardioRVARapp application."
    )
  shiny::runApp(system.file("app", package = "CardioRVARapp"), launch.browser = TRUE)
}


