#' Add commas to a numeric vector
#'
#' A wrapper for \code{prettyNum()}, and is currently used only to
#' add a comma to separate thousands places.
#'
#' @param x a numeric vector to be prettified.
#' @return a character vector with the prettified output.
#'
#' @seealso \code{\link[base]{formatC}}, \code{\link{summ}}
#'
#' @examples
#' prettify(10000)
#'
#' @export

prettify = function(x) {
  prettyNum(x, big.mark = ",", scientific = F)
}
