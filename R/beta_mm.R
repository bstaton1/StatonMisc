#'Obtain the shape parameters of a beta distribution
#'
#'Use the method of moments to obtain the \code{shape1} (alpha) and \code{shape2} (beta)
#'parameters when provided a vector assumed iid beta random variables.
#'
#'@param p a vector of iid beta random variables, for which you want
#'  to obtain the parameters of the beta distribution from which they
#'  are drawn
#'@param na.rm logical. Do you wish to remove NA elements before calculating mean and variance?
#'  Defaults to \code{FALSE}.
#'@export

beta_mm = function(p, na.rm = F) {
  # get estimates of the moments
  m = mean(p, na.rm = na.rm)
  v = var(p, na.rm = na.rm)

  # get estimates of the shape parameters
  shape1 = m * ((m * (1 - m))/v - 1)
  shape2 = (1 - m) * ((m * (1 - m))/v - 1)

  return(c(shape1 = shape1, shape2 = shape2))
}
