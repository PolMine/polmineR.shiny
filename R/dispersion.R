######################
##                  ##
##    dispersion    ##
##                  ##
######################


#' @rdname shiny_helper_functions
#' @export dispersionUiInput
dispersionUiInput <- function(){
  list(
    actionButton("dispersion_go", "Go!"),
    br(), br(),
    radioButtons("dispersion_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),
    conditionalPanel(
      condition = "input.dispersion_object == 'corpus'",
      selectInput("dispersion_corpus", "corpus", corpus()[["corpus"]])
    ),
    conditionalPanel(
      condition = "input.dispersion_object == 'partition'",
      selectInput(
        "dispersion_partition", "partition",
        choices = get("partitionNames", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)),
        selected = get("partitionNames", envir = get(".polmineR_shiny_cache", envir = .GlobalEnv))[1]
        )
    ),
    textInput("dispersion_query", "query", value="Suche"),
    selectInput(
      "dispersion_pAttribute", "pAttribute",
      choices = pAttributes(corpus()[1, "corpus"])
    ),
    # radioButtons("dispersion_dims", "number of sAttributes", choices=c("1", "2"), selected="1", inline=TRUE),
    selectInput(
      "dispersion_sAttribute_1", "sAttribute",
      choices = sAttributes(corpus()[1, "corpus"]), multiple=FALSE
    ),
    radioButtons("dispersion_ts", "time series", choices=c("yes", "no"), selected="no", inline=TRUE),
    conditionalPanel(
      condition = "input.dispersion_ts == 'yes'",
      selectInput("dispersion_ts_aggregation", "aggregation", choices=c("none", "month", "quarter", "year"), multiple = FALSE
      )
    )
  )
}


#' @rdname shiny_helper_functions
#' @export dispersionUiOutput
dispersionUiOutput <- function(){
}


#' @rdname shiny_helper_functions
#' @export dispersionServer
#' @importFrom zoo zoo
dispersionServer <- function(input, output, session){
  observe({
    x <- input$dispersion_partition
    if (x != ""){
      new_sAttr <- sAttributes(get(x, envir = get(".polmineR_shiny_cache", .GlobalEnv))@corpus)
      updateSelectInput(
        session, "dispersion_pAttribute",
        choices=pAttributes(get(x, envir = get(".polmineR_shiny_cache", .GlobalEnv))@corpus), selected=NULL
      )
      updateSelectInput(session, "dispersion_sAttribute_1", choices=new_sAttr, selected=NULL)
    }
  })
  
  observeEvent(
    input$dispersion_go,
    {
      #        if (input$dispersion_dims == "1"){
      sAttrs <- input$dispersion_sAttribute_1
      #          print(sAttrs)
      #        } else if (input$dispersion_dims == "2"){
      #          sAttrs <- c(input$dispersion_sAttribute_1, input$dispersion_sAttribute_2)
      #          print(sAttrs)
      #        }
      tab <- as.data.frame(dispersion(
        get(input$dispersion_partition),
        query=input$dispersion_query,
        sAttribute=sAttrs,
        pAttribute=input$dispersion_pAttribute
      ))
      tab[["freq"]] <- round(tab[["freq"]], 7)
      if (input$dispersion_ts == "yes"){
        if (requireNamespace("zoo", quietly=TRUE)){
          if (input$dispersion_ts_aggregation != "none"){
            dates4zoo <- tab[[input$dispersion_sAttribute_1]]
            tab[[input$dispersion_sAttribute_1]] <- NULL
            zooObject <- zoo::zoo(as.matrix(tab), order.by=as.Date(dates4zoo))
            zooObjectAggr <- switch(
              input$dispersion_ts_aggregation,
              month = zoo::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearmon(zoo::index(zooObject)))),
              quarter = zoo::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearqtr(zoo::index(zooObject)))),
              year = zoo::aggregate.zoo(zooObject, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", zoo::index(zooObject)), "-01-01", sep="")))
            )
            tab <- data.frame(date=zoo::index(zooObjectAggr), zooObjectAggr)
            colnames(tab)[1] <- input$dispersion_sAttribute_1
            rownames(tab) <- NULL
          }
          output$dispersion_plot <- renderPlot(zoo::plot.zoo(zooObjectAggr, main=""))
        } else {
          message("package 'zoo' required, but not available")
        }
      } else {
        # output$dispersion_plot <- renderPlot(as.data.frame(tab))
      }
      output$dispersion_table <- DT::renderDataTable(as.data.frame(tab))
      
    }
  )
}

