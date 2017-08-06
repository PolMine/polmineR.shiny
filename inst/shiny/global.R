library(shiny)
library(polmineR)
library(polmineR.shiny)
library(magrittr)
library(DT)
library(shinythemes)


values <- reactiveValues()
values[["partitions"]] <- list()
values[["corpora"]] <- list()
values[["fulltext"]] <- ""
values[["startingTime"]] <- as.character(Sys.time())