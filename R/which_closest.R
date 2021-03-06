#' Find Index of Closest Element
#'
#' Used to find the index of the element of a vector \code{x} with the value
#' closest to \code{y}, regardless of whether it is greater or less than \code{y}.
#'
#' @param x A numeric vector with length > 1 from which to find an element index.
#' @param y A numeric vector with length == 1, the "close to" value.
#'
#' @return a numeric vector with length 1, which represents the index of \code{x}
#'   that is closest to the value \code{y}.
#'
#' @examples
#' which_closest(x = 5:10, y = 7)
#'
#' @export

which_closest = function(x, y) {
  which.min(abs(x - y))
}

