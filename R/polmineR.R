#' Shiny apps and widgets for polmineR.
#' 
#' @export polmineR
#' @rdname polmineR_gui
#' @import polmineR
#' @import shiny
#' @import methods
#' @import magrittr
#' @importFrom shinythemes shinytheme
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniTitleBarCancelButton miniTitleBarButton
polmineR <- function(){
  # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/gui")
  runApp(system.file("shiny", package = "polmineR.shiny"))
}