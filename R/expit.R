#' Perform the Inverse-Logit Tranformation
#'
#' Performs the inverse-logit transformation.
#' See the details below for exactly what this means.
#'
#' @param x A numeric vector with length >= 1 containing the number
#' in logit space to convert to the [0,1] scale.
#'
#' @return A numeric vector with the same length as \code{x} containing the inverse-logit of \code{x}.
#'
#' @details The inverse logit tranformation turns any real number (negative to positive infinity) to
#'   a number on the [0,1] scale (e.g., the probability scale).
#'   I can never remember which transformation the built-in \code{qlogis} or \code{plogis} functions
#'   do, so I wrote my own that are more in their names.
#'
#' @seealso \code{\link{logit}}
#'
#' @examples
#' expit(x = 1)
#' expit(x = 0.5)
#' expit(x = logit(x = 0.5))
#'
#' @export

expit = function(x) {
  exp(x)/(1 + exp(x))
}
