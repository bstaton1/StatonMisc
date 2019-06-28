#' Convert Lognormal SD to CV
#'
#' Used to convert the lognormal standard deviation
#' of a random variable to the coefficient of variation
#' using a simple transformation.
#'
#' @param cv A numeric vector, possibly length > 1, containing the lognormal SD you wish to convert.
#'
#' @return A numeric vector with the same length as \code{sig},
#'   the coefficient of variation.
#'
#' @seealso \code{\link{cv2sig}}
#'
#' @examples
#' sig2cv(sig = 0.2)
#'
#' @export

sig2cv = function(sig) {
  sqrt(exp(sig^2) - 1)
}
