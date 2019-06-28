#' Determine if a year is a leap year
#'
#' Used to determine if each element in a vector of years is a leap year.
#'
#' @param date a numeric vector of length >= 1 containing the years to test
#'
#' @return a logical vector indicating whether each year is a leap year (\code{TRUE} if so, \code{FALSE} if not).
#'
#' @details a year is considered a leap year if it is divisible by 4 and not divisible by 100.
#'   The exception is years that are divisible by 400 are also considered leap years.
#'
#' @examples
#' is_leap_year(2016:2020)
#'
#' @export

is_leap_year = function(year) {
  # determine if the year is divisible by each number
  d4 = year %% 4 == 0
  d100 = year %% 100 == 0
  d400 = year %% 400 == 0

  out = logical(length(year))
  for (i in 1:length(year)) {
    if (d4[i]) {
      if (d100[i] & !d400[i]) {
        out[i] = FALSE
      } else {
        out[i] = TRUE
      }
    } else {
      out[i] = FALSE
    }
  }

  out
}
