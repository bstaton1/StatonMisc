#' Capitalize the first letter in a string
#'
#' Like \code{toupper}, but only for the first letter of a string.
#'
#' @param x a character vector of any length
#'
#' @examples
#' capitalize("ben")
#'
#' @export

capitalize = function(x) {
  # check for appropriate class
  if (!is.character(x)) stop("x must be of class 'character'")
  # extract the first letter
  first = substr(x, 1, 1)
  #extract the last bit
  last = substr(x, 2, nchar(x))
  # capitalize the first and paste on last bit
  paste0(toupper(first), last)
}
