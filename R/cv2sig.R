#' Convert CV to Lognormal SD
#'
#' This function is used to convert the coefficient of variation of a
#' random variable to the lognormal standard deviation,
#' using a simple transformation.
#'
#' @param cv a numeric vector, possibly length > 1,
#'   containing the CV to convert.
#'
#' @return a numeric vector with the same length as \code{cv},
#'   the lognormal standard deviation.
#'
#' @seealso \code{\link{sig2cv}}
#'
#' @examples
#' cv2sig(cv = 0.2)
#'
#' @export

cv2sig = function(cv) {
  sqrt(log((cv^2) + 1))
}
