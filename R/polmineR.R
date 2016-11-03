#' Shiny apps and widgets for polmineR.
#' 
#' @export polmineR
#' @rdname polmineR_gui
polmineR <- function(){
  # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/gui")
  shiny::runApp(system.file("shiny", "gui", package="polmineR"))
}