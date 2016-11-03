#' @export polmineR
polmineR <- function(){
  # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/gui")
  shiny::runApp(system.file("shiny", "gui", package="polmineR"))
}