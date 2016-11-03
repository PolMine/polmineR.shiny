
##################
##              ##
##   context    ##
##              ##
##################

#' @rdname shiny_helper_functions
#' @export contextUiInput
contextUiInput <- function(){
  list(
    actionButton("context_go", "Go!"),
    br(), br(),
    radioButtons("context_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.context_object == 'corpus'",
      selectInput("context_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.context_object == 'partition'",
      selectInput("context_partition", "partition", choices = get("partitionNames", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)))
    ),
    textInput("context_query", "query", value = ""),
    selectInput("context_pAttribute", "pAttribute:", choices=c("word", "pos", "lemma"), selected = getOption("polmineR.pAttribute"), multiple=TRUE),
    sliderInput("context_left", "left", min = 1, max = 25, value = getOption("polmineR.left")),
    sliderInput("context_right", "right", min = 1, max = 25, value = getOption("polmineR.right")),
    br()
  )
}


#' @rdname shiny_helper_functions
#' @export contextUiOutput
contextUiOutput <- function(){
  list(
    DT::dataTableOutput('context_table'),
    textOutput("context2kwic")
  )
}


#' @rdname shiny_helper_functions
#' @export contextServer
contextServer <- function(input, output, session){
  output$context_table <- DT::renderDataTable({
    input$context_go
    isolate({
      if (input$context_go > 0 && input$context_query != ""){
        
        if (input$context_object == "corpus"){
          if (!input$context_corpus %in% ls(envir = get(".corpora", .GlobalEnv))){
            withProgress(
              message = "preparing Corpus ...", value = 1, max = 1, detail = "counting",
              {assign(
                input$context_corpus,
                Corpus$new(input$context_corpus, pAttribute = input$context_pAttribute),
                envir = get(".corpora", .GlobalEnv)
              )}
              
            )
          }
          object <- get(input$context_corpus, envir = get(".corpora", .GlobalEnv))
        } else {
          object <- get(input$context_partition, envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        }
        
        withProgress(
          message = "please wait ...", value = 0, max = 6, detail = "getting started",
          {
            ctext <- context(
              .Object = object,
              query = rectifySpecialChars(input$context_query),
              pAttribute = input$context_pAttribute,
              left = input$context_left[1], right = input$context_right[1],
              verbose="shiny"
            )
            assign("ctext", ctext, envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
          })
        
        if (!is.null(ctext)){
          return(DT::datatable(round(ctext, 2)@stat, selection = "single", rownames = FALSE))
        } else {
          return(DT::datatable(data.frame(a = c(), b = c(), d = c())))
        }
      } else {
        retval <- data.frame(
          word = ""[0], count_window = ""[0], count_partition = ""[0],
          exp_window = integer(), exp_partition = integer(), ll = integer(),
          rank_ll = integer()
        )
        return(retval)
      }
    })
  })
  
  observeEvent(
    input$context_table_rows_selected,
    {
      if (length(input$context_table_rows_selected) > 0){
        ctext <- get("ctext", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        updateTextInput(
          session, "kwic_neighbor",
          value = ctext@stat[[input$context_pAttribute[1]]][input$context_table_rows_selected]
        )
        if (input$kwic_object == "partition"){
          updateSelectInput(session, "kwic_object", selected = "partition")
          updateSelectInput(session, "kwic_partition", selected = input$context_partition)
        } else if (input$kwic_object == "corpus"){
          updateSelectInput(session, "kwic_object", selected = "corpus")
          updateSelectInput(session, "kwic_corpus", selected = input$context_corpus)
        }
        updateTextInput(session, "kwic_query", value = input$context_query)
        updateSelectInput(session, "kwic_left", selected = input$context_left)
        updateSelectInput(session, "kwic_right", selected = input$context_right)
        updateSelectInput(session, "kwic_pAttribute", selected = input$context_pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")
        Time <- as.character(Sys.time())
        updateSelectInput(session, "kwic_read", choices = Time, selected = Time)
      }
    })
  

}




#' @rdname polmineR_gui
setMethod("context", "missing", function(){
  if (requireNamespace("shiny", quietly=TRUE)){
    shiny::runApp(system.file("shiny", "context", package="polmineR"), launch.browser=TRUE)  
    # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/context", launch.browser=TRUE)
  } else {
    message("package shiny not available")
  }
})


