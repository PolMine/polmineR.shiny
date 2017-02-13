#' @rdname shiny_helper_functions
#' @export countUiInput
countUiInput <- function(){
  list(
    go = actionButton("count_go", label="", icon = icon("play", lib = "glyphicon")),
    radioButtons("count_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.count_object == 'corpus'",
      selectInput("count_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.count_object == 'partition'",
      selectInput("count_partition", "partition", choices = partition(get(".polmineR_shiny_cache", .GlobalEnv))[["object"]])
    ),
    textInput("count_query", "query", value = "")
  )
}


#' @rdname shiny_helper_functions
#' @export countUiOutput
countUiOutput <- function(){
  DT::dataTableOutput('count_table')
}


#' @rdname shiny_helper_functions
#' @export countServer
countServer <- function(input, output, session){

  output$count_table <- DT::renderDataTable({
    input$count_go
    isolate({
      if (input$count_go > 0 && input$count_query == ""){
        
        if (input$count_object == "corpus"){
          if (!input$count_corpus %in% ls(envir = get(".corpora", .GlobalEnv))){
            withProgress(
              message = "preparing Corpus ...", value = 1, max = 1, detail = "counting",
              {
                assign(
                input$count_corpus,
                Corpus$new(input$count_corpus, pAttribute = input$count_pAttribute),
                envir = get(".corpora", .GlobalEnv)
              )
                }
              
            )
          }
        }
        
        .Object <- switch(
          input$count_object,
          corpus = get(input$count_corpus, envir = get(".corpora", .GlobalEnv)),
          partition = get(input$count_partition, envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        )
        
        if (input$count_query == ""){
          return(getSlot(.Object, "stat"))
        } else {
          return(count(.Object, query = input$count_query))
        }

      } else {
        return(data.frame(word = ""[0], count = integer()))
      }
    })
  })
}
