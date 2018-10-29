#' Perform the Logit Tranformation
#'
#' This function performs the logit transformation.
#' See the details below for exactly what this means.
#'
#' @param x A numeric vector, possibly length > 1, containing the number
#' in the interval from [0,1] to convert to logit space.
#'
#' @return A numeric vector with the same length as \code{x} containing the logit of \code{x}.
#'
#' @details The logit tranformation turns a number in the interval [0,1] (e.g., the probability scale)
#'   to logit-space, which ranges from negative to positive infinity. It is also known as
#'   the log-odds transformation, and is a commonly used link function in GLMs using binary data.
#'
#' @seealso \code{\link{expit}}
#'
#' @examples
#' logit(x = 0.5)
#' logit(x = 0.2)
#' logit(x = c(0.2, 0.5, 0.7))
#'
#' @export

logit = function(x) {
  log(x/(1 - x))
}
