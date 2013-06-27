# Extract faceting variables
# 
# Used within \code{pitchRx} plotting functions
# 
# @param layer ggplot2 layer(s) from the original plotting call
# @result character vector with variable names (if faceting is present). Otherwise, \code{NULL}
# 
getFacets <- function(layer){
  layers <- unlist(layer)
  if (length(layers) == 0) return(NULL)
  rows <- layers$rows
  cols <- layers$cols
  if (length(rows) > 0) {
    row.facet <- as.character(rows)
  } else {
    row.facet <- NULL
  }
  if (length(cols) > 0) {
    col.facet <- as.character(cols)
  } else {
    col.facet <- NULL
  }
  return(c(row.facet, col.facet))
}
