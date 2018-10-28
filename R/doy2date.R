#' Convert Date to DOY
#'
#' This function is used to convert a vector of dates formated as M-D-YYYY
#' to a vector of days of the year. The calculation automatically takes
#' leap years into account.
#'
#' @param doy a numeric vector of length 1 specifying the day of the year to convert
#' @param year a numeric vector of length 1 specifying the year \code{doy} corresponds to.
#'   This is necessary to account for leap years
#' @param leading_zero logical: do you wish to insert a zero in front of the month and day in the date
#'   if they are just single numbers (e.g., \code{"6/1"} would become \code{"06/01"} if set to TRUE).
#'   Defaults to \code{FALSE}
#' @param include_year logical: do you wish to include the year in the output?
#'   Defaults to \code{TRUE}
#'
#' @return return a character vector containing the converted dates.
#'
#' @seealso \code{\link{date2doy}}
#'
#' @examples
#' doy2date(51, 2016)
#' date2doy(51, 2017, TRUE)
#'
#' @export

doy2date = function(doy, year, leading_zero = FALSE, include_year = TRUE) {

  if(missing(year)) stop("Must specify the year, otherwise you can't account for leap years!")
  if(missing(doy)) stop("Must specify the doy, otherwise you can't get a date!")
  if(length(year) > 1 | length(doy) > 1) stop("'doy' and 'year' must be vectors with length == 1")

  # make keys
  # leap year key
  dates = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), 1)
  dates = gsub(x = dates, pattern = "2016-", replacement = "")
  dates = gsub(x = dates, pattern = "-", replacement = "/")
  month.key = as.integer(substr(dates, 1, 2))
  day.key = as.integer(substr(dates, 4, 5))
  leap.key = data.frame(day = day.key, month = month.key, doy = 1:366)

  # common year key
  dates = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), 1)
  dates = gsub(x = dates, pattern = "2015-", replacement = "")
  dates = gsub(x = dates, pattern = "-", replacement = "/")
  month.key = as.integer(substr(dates, 1, 2))
  day.key = as.integer(substr(dates, 4, 5))
  common.key = data.frame(day = day.key, month = month.key, doy = 1:365)

  if (doy > 365 & !is_leap_year(year)) stop("There is no 366th day in a common year!")
  if (is_leap_year(year)) key = leap.key else key = common.key

  date.info = key[key$doy == round(doy),]

  day = as.character(date.info$day)
  month = as.character(date.info$month)

  if (leading_zero) {
    if(nchar(day) < 2) day = paste("0", day, sep = "")
    if(nchar(month) < 2) month = paste("0", month, sep = "")
  }

  if (include_year) {
    date = paste(month, day, year, sep = "/")
  } else {
    date = paste(month, day, sep = "/")
  }

  return(date)
}
