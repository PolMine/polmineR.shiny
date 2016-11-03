library(shiny)
library(polmineR)
library(polmineR.shiny)
library(magrittr)
library(DT)
library(shinythemes)

assign(
  "startingTime", as.character(Sys.time()),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)

assign(
  "fulltext", "",
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)

if (!".corpora" %in% names(.GlobalEnv)){
  message("... creating environment '.corpora'")
  .corpora <- new.env(parent = .GlobalEnv)
}

assign(
  "partitionNames",
  c(getObjects('partition'), getObjects('pressPartition'), getObjects('plprPartition')),
  envir = get(".polmineR_shiny_cache", envir = .GlobalEnv)
)
