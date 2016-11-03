.onAttach <- function(lib, pkg){
  if (!".polmineR_cache" %in% ls(.GlobalEnv, all.names = TRUE)){
    assign(".polmineR_cache", value = new.env(), envir = .GlobalEnv)
  }
  setTemplate()
}
