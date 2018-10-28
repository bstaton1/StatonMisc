#' Convert a decimal time to a nice-looking time
#'
#' Goes from hours.fraction to hours:minutes.
#'
#' @param dec_times a numeric vector with length >= 1,
#'   and values in the interval [0,24].
#' @param clock a character vector specifying if you want a 12-hour clock
#'   (\code{"12hr"}) or a 24-hour clock(i.e., miliary time; \code{"24hr"})
#'
#' @return a character vector with the converted times.
#'   If \code{clock == "12hr"}, either \code{"AM"} or \code{"PM"}
#'   will be included at the end of the character string in each element.
#'
#' @note Any cases where \code{dec_times == 0 | dec_times == 24} will be
#'   converted to \code{"12:00AM"} if \code{clock == "12hr"} and
#'   \code{"0:00"} if \code{clock == "24hr"}.
#'
#' @examples
#' dec_time2nice_time(c(2.25, 14.55), "12hr")
#' dec_time2nice_time(c(2.25, 14.55), "24hr")

dec_time2nice_time = function(dec_times, clock = "12hr") {

  # error check
  if (!(clock %in% c("12hr", "24hr"))) {
    stop("clock must be one of '12hr' or '24hr'")
  }

  if (any(dec_times < 0 | dec_times > 24)) {
    stop("invalid values for dec_times specified: outside the interval [0,24]")
  }

  # get the completed hours portion
  hours = floor(dec_times)

  # get the partial hours portion
  decimals = dec_times - hours

  # format the minutes
  minutes = round((decimals) * 60)
  minutes = as.character(minutes)
  minutes[minutes == "0"] = "00"
  minutes = ifelse(nchar(minutes) == 1, paste("0", minutes, sep = ""), minutes)

  if (clock == "24hr") {
    nice_times = paste(hours, minutes, sep = ":")
    nice_times = ifelse(nice_times == "24:00", "0:00", nice_times)
  } else {
    am.pm = ifelse(hours >= 12 & hours != 24, "PM", "AM")
    hours = ifelse(am.pm == "PM" & hours != 12, hours - 12, hours)
    nice_times = paste(paste(hours, minutes, sep = ":"),am.pm, sep = "")
    nice_times = ifelse(hours == 24 | nice_times == "0:00AM", "12:00AM", nice_times)
  }
  nice_times
}

