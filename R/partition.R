#' Building blocks for shiny apps and widgets.
#' 
#' 
#' @param input input
#' @param output output
#' @param session session
#' @rdname shiny_helper_functions
#' @export partitionUiInput
#' @import shiny
partitionUiInput <- function(){
  list(
    go = actionButton("partition_go", label="", icon=icon("play", lib="glyphicon")),
    br(),
    br(),
    corpus = selectInput("partition_corpus", "corpus", choices = corpus(), selected = corpus()[1]),
    name = textInput(inputId = "partition_name", label = "name", value = "UNDEFINED"),
    sAttributesA = selectInput(
      inputId = "partition_sAttributes", label = "sAttributes", multiple = TRUE,
      choices = sAttributes(corpus()[1, "corpus"])
    ),
    sAttributesB = uiOutput("partition_sAttributes"),
    pAttribute = selectInput(inputId = "partition_pAttribute", label = "pAttribute", multiple = TRUE, choices = list(none = "", word = "word", lemma = "lemma")),
    regex = radioButtons("partition_regex", "regex", choices = list("TRUE", "FALSE"), inline = TRUE),
    xml = radioButtons("partition_xml", "xml", choices = list("flat", "nested"), inline = TRUE)
  )
}


#' @rdname shiny_helper_functions
#' @export partitionUiOutput
partitionUiOutput <- function(){
  dataTableOutput('partition_table')
}


#' @rdname shiny_helper_functions
#' @export partitionServer
partitionServer <- function(input, output, session){
  observeEvent(
    input$partition_go,
    {
      selectInputToUpdate <- c("kwic_partition", "context_partition", "dispersion_partition")
      if (input$partition_go > 0){
        defList <- lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) gsub('\u201E', '"', input[[x]]) 
        )
        assign(
          input$partition_name,
          partition(
            as.character(input$partition_corpus),
            def = defList,
            name = input$partition_name,
            pAttribute = input$partition_pAttribute,
            regex = input$partition_regex,
            xml = input$partition_xml,
            mc = FALSE,
            verbose = TRUE
          ),
          envir=.GlobalEnv
        )
        partitionDf <- partition()
        if (nrow(partitionDF) == 0){
          partitionDF <- data.frame(
            object = ""[0], name = ""[0], corpus = ""[0], size = integer()
          )
        } 
        rownames(partitionDf) <- NULL
        output$partition_table <- DT::renderDataTable(partitionDf)
        
        for (toUpdate in selectInputToUpdate) {
          updateSelectInput(session, toUpdate, choices=partitionDf$object, selected=NULL)  
        }
      }
    }
  )
  
  observeEvent(
    input$partition_corpus,
    {
      updateSelectInput(
        session, inputId = "partition_sAttributes",
        choices = sAttributes(input$partition_corpus)
      )
    }
  )
  
  output$partition_sAttributes <- renderUI({
    tagList(lapply(
      input$partition_sAttributes,
      function(x) textInput(x, x)
    ))
  })
  
  retval <- observeEvent(
    input$partition_done,
    {
      newPartition <- partition(
        as.character(input$partition_corpus),
        def = lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) gsub('\u201E', '"', input[[x]]) 
        ),
        name = input$partition_name,
        pAttribute = input$partition_pAttribute,
        regex = input$partition_regex,
        xml = input$partition_xml,
        mc = FALSE,
        verbose = TRUE
      )
      stopApp(returnValue = newPartition)
      
    }
  )
  
  output$partition_table <- renderDataTable(partition())
  
}

