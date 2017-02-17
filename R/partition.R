#' Building blocks for shiny apps and widgets.
#' 
#' 
#' @param input input
#' @param output output
#' @param session session
#' @param drop elements to drop
#' @param ... further parameters
#' @rdname shiny_helper_functions
#' @export partitionUiInput
#' @import methods
#' @importFrom DT renderDataTable dataTableOutput
partitionUiInput <- function(){
  list(
    go = actionButton("partition_go", label="", icon = icon("play", lib="glyphicon")),
    br(),
    br(),
    corpus = selectInput("partition_corpus", "corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1]),
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
  DT::dataTableOutput('partition_table')
}


#' @rdname shiny_helper_functions
#' @export partitionServer
partitionServer <- function(input, output, session){
  observeEvent(
    input$partition_go,
    {
      
      if (input$partition_go > 0){
        defList <- lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) input[[x]]
        )
        withProgress(
          message = "please wait ...", value = 0, max = 3, detail = "getting started",
          {
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
                verbose = "shiny"
              ),
              envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
            )
          }
        )
        partitionDf <- partition(get(".polmineR_shiny_cache", envir = .GlobalEnv))
        if (nrow(partitionDf) == 0){
          partitionDF <- data.frame(
            object = ""[0], name = ""[0], corpus = ""[0], size = integer()
          )
        } else {
          rownames(partitionDf) <- NULL  
        }
        output$partition_table <- DT::renderDataTable(partitionDf)
        
        selectInputToUpdate <- c("kwic_partition", "context_partition", "dispersion_partition", "features_partition_x", "features_partition_y", "count_partition")
        for (toUpdate in selectInputToUpdate) {
          updateSelectInput(session, toUpdate, choices = partitionDf$object, selected = NULL)
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
      function(x){
        selectInput(
          inputId = x, label = x, multiple = TRUE,
          choices = sAttributes(input$partition_corpus, x)
          )
      } 
    ))
  })
  
  retval <- observeEvent(
    input$partition_done,
    {
      newPartition <- partition(
        as.character(input$partition_corpus),
        def = lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          rectifySpecialChars
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
  
  output$partition_table <- DT::renderDataTable(
    data.frame(object = ""[0], name = ""[0], corpus = ""[0], size = integer())
    )
  
}

#' @export partitionGadget
#' @rdname polmineR_gui
partitionGadget <- function(){
  partitionGadgetUI <- miniPage(
    theme = shinytheme("cerulean"),
    gadgetTitleBar(
      "Create partition",
      left = miniTitleBarCancelButton(),
      right = miniTitleBarButton(inputId = "partition_done", label = "Go", primary = TRUE)
    ),
    miniContentPanel(
      fillPage(
        fillRow(
          fillCol(
            div(partitionUiInput()[["corpus"]],
                partitionUiInput()[["name"]],
                partitionUiInput()[["pAttribute"]],
                partitionUiInput()[["regex"]],
                partitionUiInput()[["xml"]]
            )
          ),
          fillCol(br()),
          fillCol(
            div(
              partitionUiInput()[["sAttributesA"]],
              partitionUiInput()[["sAttributesB"]]
            )
          ),
          flex = c(1,0.1, 1)
        )
        
      ),
      padding = 10
    )
  )
  
  returnValue <- runGadget(
    app = shinyApp(
      ui = partitionGadgetUI,
      server = partitionServer
    ),
    viewer = paneViewer()
  )
  return(returnValue)
  
}
