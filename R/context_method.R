#' @rdname context-method
setMethod("context", "missing", function(){
  if (requireNamespace("shiny", quietly=TRUE)){
    shiny::runApp(system.file("shiny", "context", package="polmineR"), launch.browser=TRUE)  
    # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/context", launch.browser=TRUE)
  } else {
    message("package shiny not available")
  }
})


