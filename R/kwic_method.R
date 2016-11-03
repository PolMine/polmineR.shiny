#' @rdname kwic
setMethod("kwic", "missing", function(){
  
  if (require("miniUI", quietly = TRUE) && require("shinythemes", quietly = TRUE) && require("shiny", quietly = TRUE) && require("magrittr", quietly = TRUE)){
    
    assign(
      "partitionNames",
      value = unlist(sapply(c("partition", "pressPartition", "plprPartition"), getObjects)),
      envir = .GlobalEnv
    )
    
    kwicGadgetUI <- shinyUI(fluidPage(
      theme = shinytheme("cerulean"),
      padding = 5,
      gadgetTitleBar(
        "KWIC",
        left = miniTitleBarCancelButton(),
        right = miniTitleBarButton(inputId = "kwic_go", label = "Go", primary = TRUE)
      ),
      div(br()),
      sidebarLayout(
        sidebarPanel = sidebarPanel(.kwicUiInput(drop = c("go", "br1", "br2", "pAttribute", "read"))),
        mainPanel = mainPanel(.kwicUiOutput())
      )
    ))
    
    returnValue <- runGadget(
      app = shinyApp(ui = kwicGadgetUI, server = .kwicServer),
      viewer = browserViewer()
    )
    return(returnValue)
    
  } else {
    warning("One of the following packages is missing: miniUI, shinythemes, shiny")
  }
})
