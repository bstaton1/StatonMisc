#' Opposite of \code{\%in\%}
#'
#' Sometimes is nice to have a shortcut.
#'
#' @name grapes_not_in_grapes
#' @aliases not_in
#' @param x A vector.
#' @param y A vector.
#'
#' @details The infix operator \code{\%in\%} tests whether elements in one vector
#'   are also found in another vector. To negate this test, users previously needed to wrap
#'   the whole statement in parentheses. This helps to get around this hassle.
#'
#' @examples
#' v = c("a", "b", "c"); "x" %!in% v
#'
#' @export

`%!in%` = function(x, y) {
  !(x %in% y)
}
