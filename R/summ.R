#' Summarize a Vector of Quantities
#'
#' This function is a wrapper for \code{mean()}, \code{quantile()},
#' \code{sd}, \code{round()}, and \code{prettyNum()}. So it can easily
#' be called with less code. Also, nice to be able place these calls in
#' \code{apply()} call.
#'
#' @param x A numeric vector with length > 1. It can have \code{NA} elements.
#' @param p A numeric vector with elements on the interval (0,1).
#'   The quantile points requested. Defaults to \code{c(0.5, 0.025, 0.975)}.
#' @param rnd A numeric vector of length == 1. The rounding specification.
#'   Defaults to \code{NULL}, i.e., no rounding.
#' @param na.rm logical. Do you wish to remove \code{NA} elements before calculating summary statistics?
#'   Defaults to \code{FALSE}.
#' @param prettify logical. Do you wish to insert commas in the output? If so,
#'   it will be coerced to a character vector. Defaults to \code{FALSE}.
#' @return A vector with length depending on the requested quantiles.
#'   If \code{isTRUE(prettify)}, output will be a named character vector, otherwise,
#'   it will be a named numeric vector.
#'
#' @seealso \code{\link[base]{round}}, \code{\link[stats]{quantile}}
#'
#' @examples
#' summ(x = rnorm(100, 50000, 5000))
#' summ(x = rnorm(100, 50000, 5000),
#'      p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
#'      rnd = -3, prettify = T)
#' @export

summ = function(x,
                p = c(0.5, 0.025, 0.975),
                rnd = NULL, na.rm = F, prettify = FALSE) {

  # calculate the main summary
  out = c(mean = mean(x, na.rm = na.rm),
          sd = sd(x, na.rm = na.rm),
          quantile(x, p, na.rm = na.rm)
  )

  # if rounding, do it
  if (!is.null(rnd)) {
    out = round(out, rnd)
  }

  # if prettifying, do it
  if(prettify) {
    out = prettify(out)
  }

  # return the output
  out
}
