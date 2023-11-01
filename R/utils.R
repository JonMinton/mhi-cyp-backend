
#' Inverse logit
#'
#' @param x a number between -infinity and + infinity
#'
#' @return the inverse logit of the input x
#' @export
#'
#' @examples
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}
