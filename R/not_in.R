#' Opposite of \code{\%in\%}
#'
#' Sometimes is nice to have a shortcut.
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @details the infix operator \code{\%in\%} tests whether elements in one vector
#'   are also found in another vector. To negate this test, previously needed to wrap
#'   the whole statement in parentheses. This helps you get around this hassle.
#'
#' @examples
#' v = c("a", "b", "c"); "x" %!in% v
#'
#' @export

`%!in%` = function(x, y) {
  !(x %in% y)
}
