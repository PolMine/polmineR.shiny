##################
##              ##
##    kwic      ##
##              ##
##################


#' @rdname shiny_helper_functions
#' @importFrom DT formatStyle
#' @export kwicUiInput
kwicUiInput <- function(drop = NULL){
  divs = list(
    go = actionButton("kwic_go", "", icon = icon("play", lib = "glyphicon")),
    mail = actionButton("kwic_mail", "", icon = icon("envelope", lib = "glyphicon")),
    br1 = br(),
    br2 = br(),
    object = radioButtons("kwic_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    corpus = conditionalPanel(
      condition = "input.kwic_object == 'corpus'",
      selectInput("kwic_corpus", "corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1])
    ),
    partition = conditionalPanel(
      condition = "input.kwic_object == 'partition'",
      selectInput("kwic_partition", "partition", choices = get("partitionNames", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)))
    ),
    query = textInput("kwic_query", "query", value = ""),
    neighbor = textInput("kwic_neighbor", "neighbor", value = ""),
    sAttribute = selectInput(
      "kwic_meta", "sAttribute",
      choices = sAttributes(corpus()[["corpus"]][1]),
      multiple = TRUE
    ),
    pAttribute = selectInput(
      "kwic_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[["corpus"]][1])
    ),
    left = sliderInput("kwic_left", "left", min = 1, max = 25, value = getOption("polmineR.left")),
    right = sliderInput("kwic_right", "right", min = 1, max = 25, value = getOption("polmineR.right")),
    read = conditionalPanel(
      condition = "input.kwic_go == -1",
      selectInput("kwic_read", "read", choices = Sys.time())
    ),
    br3 = br()
  )
  if (!is.null(drop)) for (x in drop) divs[[x]] <- NULL
  divs
}

#' @rdname shiny_helper_functions
#' @export kwicUiOutput
kwicUiOutput <- function(){
  DT::dataTableOutput('kwic_table')
}


#' @rdname shiny_helper_functions
#' @importFrom DT formatStyle datatable
#' @export kwicServer
kwicServer <- function(input, output, session, ...){
  
  observe({
    x <- input$kwic_partition
    if (x != ""){
      new_sAttr <- sAttributes(get(x, envir = get(".polmineR_shiny_cache", .GlobalEnv))@corpus)
      new_pAttr <- pAttributes(get(x, envir = get(".polmineR_shiny_cache", .GlobalEnv))@corpus)
      updateSelectInput(session, "kwic_pAttribute", choices = new_pAttr, selected = NULL)
      updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
    }
  })
  
  observe({
    x <- input$kwic_corpus
    new_sAttr <- sAttributes(input$kwic_corpus)
    new_pAttr <- pAttributes(input$kwic_corpus)
    updateSelectInput(session, "kwic_pAttribute", choices = new_pAttr, selected = NULL)
    updateSelectInput(session, "kwic_meta", choices = new_sAttr, selected = NULL)
  })
  
  output$kwic_table <- DT::renderDataTable({
    
    input$kwic_go
    input$kwic_read
    
    isolate({
      
      startingTime <- get("startingTime", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
      if ((input$kwic_go > 0 || input$kwic_read != startingTime) && input$kwic_query != ""){
        
        if (input$kwic_object == "corpus"){
          object <- input$kwic_corpus
        } else {
          object <- get(input$kwic_partition, get(".polmineR_shiny_cache", envir = .GlobalEnv))
        }
        
        withProgress(
          message="please wait", value = 0, max = 5, detail="preparing data",
          {
            kwicObject <- polmineR::kwic(
              .Object = object,
              query = rectifySpecialChars(input$kwic_query),
              pAttribute = ifelse(is.null(input$kwic_pAttribute), "word", input$kwic_pAttribute),
              left = input$kwic_left,
              right = input$kwic_right,
              meta = input$kwic_meta,
              verbose = "shiny",
              neighbor = input$kwic_neighbor,
              cpos = TRUE # required for reading
            )
            assign(
              "kwicObject", kwicObject,
              envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
              )
          }
        ) # end withProgress
        
        if (is.null(kwicObject)){
          tab <- data.frame(left = ""[0], node = ""[0], right = ""[0])
        } else {
          tab <- kwicObject@table
        }
        
        if (length(input$kwic_meta) == 0 && nrow(tab) > 0){
          retval <- data.frame(no = c(1:nrow(tab)), tab)
        } else if (length(input$kwic_meta) > 0){
          metaRow <- unlist(lapply(
            c(1:nrow(tab)),
            function(i){
              paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
            }
          ))
          retval <- data.frame(meta = metaRow, tab[,(length(input$kwic_meta)+1):ncol(tab)])
        }
        
      } else {
        retval <- data.frame(left = ""[0], node = ""[0], right = ""[0])
      }
      
    })
    
    # format DataTable
    retval <- DT::datatable(retval, selection = "single", rownames = FALSE)
    retval <- DT::formatStyle(retval, "node", color = "#4863A0", textAlign = "center")
    retval <- DT::formatStyle(retval, "left", textAlign = "right")
    if (length(input$kwic_meta) > 0){
      retval <- DT::formatStyle(
        retval, "meta", fontStyle = "italic", textAlign = "left", borderRight = "1px solid DarkGray")
    }
    retval
  })
  
  observeEvent(
    input$kwic_table_rows_selected,
    {
      if (length(input$kwic_table_rows_selected) > 0){
        kwicObject <- get("kwicObject", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        fulltext <- html(kwicObject, input$kwic_table_rows_selected, type = "plpr", verbose = TRUE)
        fulltext <- htmltools::HTML(gsub("<head>.*?</head>", "", as.character(fulltext)))
        fulltext <- htmltools::HTML(gsub('<blockquote>', '<blockquote style="font-size:14px">', as.character(fulltext)))
        output$read_fulltext <- renderUI(fulltext)
        
        updateNavbarPage(session, "polmineR", selected = "read")
      }
    })
  
  observeEvent(
    input$kwic_mail,
    {
      if (input$kwic_mail > 0){
        polmineR:::mail(
          get("kwicObject", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))
        )
      }
    }
  )
  
  
}




#' @rdname polmineR_gui
setMethod("kwic", "missing", function(){
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
        sidebarPanel = sidebarPanel(kwicUiInput(drop = c("go", "br1", "br2", "pAttribute", "read"))),
        mainPanel = mainPanel(kwicUiOutput())
      )
    ))
    
    returnValue <- runGadget(
      app = shinyApp(ui = kwicGadgetUI, server = kwicServer),
      viewer = browserViewer()
    )
    return(returnValue)
    
})
