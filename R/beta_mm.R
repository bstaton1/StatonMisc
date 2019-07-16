#' Obtain the shape parameters of a beta distribution
#'
#' Use the method of moments to obtain the \code{shape1} (alpha) and \code{shape2} (beta)
#' parameters when provided either a vector of beta random variables
#' or the sample mean and variance of such a vector.
#'
#' @param mn numeric vector of length == 1. The mean of a beta random variable.
#'   Must be specified if \code{vr} is not \code{NULL}
#' @param vr numeric vector of length == 1. The variance of a beta random variable.
#'  Must be specified if \code{mn} is not \code{NULL}
#' @param p numeric vector of length > 1. iid beta random variables.
#'   Must be \code{NULL} if \code{mn} and \code{vr} are not \code{NULL}
#' @param na.rm logical. Do you wish to remove NA elements before calculating mean and variance?
#'   Defaults to \code{FALSE} and only takes effect if \code{p} is not \code{NULL}.
#' @examples
#' beta_mm(p = rbeta(1e6, 6, 1))
#' beta_mm(mn = 0.5, vr = 0.04)
#' @export

beta_mm = function(mn = NULL, vr = NULL, p = NULL, na.rm = FALSE) {

  yes_mn = !is.null(mn)
  yes_vr = !is.null(vr)
  yes_p = !is.null(p)

  # calculate the moment estimates if p supplied
  if (!yes_mn & !yes_vr & yes_p) {
    mn = mean(p, na.rm = na.rm)
    vr = var(p, na.rm = na.rm)
  } else {
    if ((yes_mn | yes_vr) & yes_p) {
      stop ("can't specify 'mn' or 'vr' along with 'p'")
    } else {
      if ((yes_mn & !yes_vr) | (!yes_mn & yes_vr)) {
        stop ("if specifying either 'mn' or 'vr' you must supply both")
      }
    }
  }

  # get estimates of the shape parameters
  shape1 = mn * ((mn * (1 - mn))/vr - 1)
  shape2 = (1 - mn) * ((mn * (1 - mn))/vr - 1)

  return(c(shape1 = shape1, shape2 = shape2))
}
