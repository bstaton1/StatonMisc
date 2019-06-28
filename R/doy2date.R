#' Convert DOYs to Dates
#'
#' Used to convert a vector of days of the year into
#' a calendar date format, taking leap years into account.
#'
#' @param doy A numeric vector specifying the day of the year to convert.
#' @param year A numeric vector specifying the year \code{doy} corresponds to.
#'   This is necessary to account for leap years. If only one element is provided
#'   it will be assumed that year corresponds to all DOY values provided to \code{doy}.
#'   Otherwise, \code{year} and \code{doy} must be the same length, and they will be matched elementwise.
#' @param leading_zero logical: Do you wish to insert a zero in front of the month and day in the date
#'   if they are just single numbers (e.g., \code{"6/1"} would become \code{"06/01"} if set to \code{TRUE}).
#'   Defaults to \code{FALSE}.
#' @param include_year logical: Do you wish to include the year in the output?
#'   Defaults to \code{TRUE}.
#'
#' @return A character vector containing the converted dates.
#'
#' @seealso \code{\link{date2doy}}
#'
#' @examples
#' doy2date(doy = c(151,152), year = 2016)
#' doy2date(doy = c(151,151), year = c(2015, 2016))
#' date2doy(51, 2017, TRUE)
#'
#' @export

doy2date = function(doy, year, leading_zero = FALSE, include_year = TRUE) {

  if (missing(year)) stop("Must specify the year, otherwise you can't account for leap years!")
  if (missing(doy)) stop("Must specify the doy, otherwise you can't get a date!")
  if (length(doy) > 1 & length(year) == 1) year = rep(year, length(doy))
  if ((length(doy) > 1 & length(year) > 1) & (length(doy) != length(year))) {
    stop("If both 'doy' and 'year' are vectors of length > 1, they must have the same length")
  }

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

  if (any(doy > 365 & !is_leap_year(year))) stop("The 366th day is only allowed in leap years!")

  day = numeric(length(doy))
  month = numeric(length(doy))
  for (i in 1:length(doy)) {
    if (is_leap_year(year[i])) key = leap.key else key = common.key
    date.info = key[key$doy == round(doy[i]),]
    day[i] = as.character(date.info$day)
    month[i] = as.character(date.info$month)
  }

  if (leading_zero) {
    day = ifelse(nchar(day) < 2, paste("0", day, sep = ""), day)
    month = ifelse(nchar(month) < 2, paste("0", month, sep = ""))
  }

  if (include_year) {
    date = paste(month, day, year, sep = "/")
  } else {
    date = paste(month, day, sep = "/")
  }

  return(date)
}
