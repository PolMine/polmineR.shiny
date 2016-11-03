shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    tabPanel(
      "partition",
      sidebarLayout(
        sidebarPanel = sidebarPanel(partitionUiInput()),
        mainPanel = mainPanel(partitionUiOutput())
      )),
    
    tabPanel(
      "kwic",
      sidebarLayout(
        sidebarPanel = sidebarPanel(kwicUiInput()),
        mainPanel = mainPanel(kwicUiOutput())
      )
    ),
    
    tabPanel(
      "context",
      sidebarLayout(
        sidebarPanel = sidebarPanel(contextUiInput()),
        mainPanel = mainPanel(contextUiOutput())
      )
    ),
    
    tabPanel(
      "read",
      fluidPage(
        fluidRow(
          column(2),
          column(8, readUiOutput()),
          column(2)
        )
      )
    ),
    
    tabPanel(
      "dispersion",
      sidebarLayout(
        sidebarPanel = sidebarPanel(dispersionUiInput()),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel("Table", DT::dataTableOutput('dispersion_table')),
            tabPanel("Plot", plotOutput('dispersion_plot'))
          )
        )
      )
    )
    
   )
)