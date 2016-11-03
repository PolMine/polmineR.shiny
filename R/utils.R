#' Rectify special characters.
#' 
#' @param x character vector to work on
#' @export rectifySpecialCharse
rectifySpecialChars <- function(x){
  x <- gsub('\u201E', '"', x)
  x <- gsub('\u201C', '"', x) 
  x
} 