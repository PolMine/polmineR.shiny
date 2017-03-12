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


values <- reactiveValues()
values[["partitions"]] <- list()
values[["corpora"]] <- list()
values[["fulltext"]] <- ""
