.onAttach <- function(lib, pkg){
  if (!".polmineR_shiny_cache" %in% ls(.GlobalEnv, all.names = TRUE)){
    assign(".polmineR_shiny_cache", value = new.env(), envir = .GlobalEnv)
  }
}
