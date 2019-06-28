#' Convert Dates to DOYs
#'
#' Used to convert a vector of dates formated as M-D-YYYY
#' to a vector of days of the year, while taking leap years into account.
#'
#' @param date a character vector containing the date to convert. June 1, 2018 would be \code{"6/1/2018"},
#'   and December 25, 1975 would be \code{"12/25/1975"}. Other date formats are not currently accepted
#'   although dates from more than one year can be included.
#'
#' @return a numeric vector containing the DOY that was converted from M-D-YYYY format.
#'
#' @seealso \code{\link{doy2date}}
#'
#' @examples
#' date2doy("6/12/2016")
#' date2doy(c("6/12/2017", "6/19/2017"))
#' date2doy(c("6/1/2016", "6/1/2017"))
#'
#' @export

date2doy = function(date) {
  # first, split up dates
  date.split = matrix(unlist(strsplit(as.character(date), "/")),
                      nrow = length(date), ncol = 3, byrow = T)
  date.split = data.frame(day = as.numeric(date.split[,2]),
                          month = as.numeric(date.split[,1]),
                          year = as.numeric(date.split[,3]))

  year = date.split$year

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

  n = nrow(date.split)
  doys = numeric(n)
  for (i in 1:n) {
    if (is_leap_year(year[i])) key = leap.key else key = common.key
    doys[i] = key$doy[key$month == date.split$month[i] & key$day == date.split$day[i]]
  }

  return(doys)
}
