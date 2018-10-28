#' Determine if a year is a leap year
#'
#' This function is used to determine if each element in a vector of years is
#' a leap year.
#'
#' @param date a numeric vector of length >= 1 containing the years to test
#'
#' @return a logical vector indicating whether each year is a leap year (TRUE if so, FALSE if not).
#'
#' @examples
#' is_leap_year(2016:2020)

is_leap_year = function(year) {
  # function to determine if x is a whole number
  is_whole_number = function(x) {
    x == round(x)
  }

  yr_type = ifelse(!is_whole_number(year/4), "common",
                   ifelse(!is_whole_number(year/100), "leap",
                          ifelse(!is_whole_number(year/400), "common", "leap")))

  yr_type == "leap"
}
