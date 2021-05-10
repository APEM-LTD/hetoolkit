
#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
fbind <- function(a, b) {
  factor(c(as.character(a), as.character(b)))
}
