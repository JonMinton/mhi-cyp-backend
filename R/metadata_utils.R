# Functions for interrogating the contents of metadata

#' Check if an indicator is dataless
#'
#' @param x list object with metadata
#'
#' @return boolean
#' @export
#'
#' @examples
isDataless <- function(x){
  x$metadata$dataless
}
