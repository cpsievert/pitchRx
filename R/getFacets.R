#' Extract faceting variables
#' 
#' Used within \code{pitchRx} plotting functions
#' 
#' @param layers ggplot2 layers from the original plotting call
#' @result character vector with variable names (if faceting is present). Otherwise, \code{NULL}
#' 
getFacets <- function(layers){
  if (length(grep("facet", layers) > 0)) {
    if (length(layers) > 2) {
      facet <- layers[grep("facet", layers)]
      facets <- gsub("[)]", "", gsub("facet_[a-z]+[(]","", facet))
    } else facets <- layers[-grep("facet", layers)]
    facets2 <- llply(str_split(as.character(facets), "~"), str_trim)
    facets3 <- unlist(llply(facets2, function(x) { x[!x %in% "."] }))
  } else facets3 <- NULL
  return(facets3)
}