#' Print a basic progress indicator to console
#'
#' Include this function in a loop or other calculation that iterates over elements
#' in which both the current and total number of elements is known.
#'
#' @param i numeric vector of length == 1. The current iteration value
#' @param n numeric vector of length == 1. The number of iterations to calculate
#' @param grp character or numeric vector of length == 1. Optional additional identifier to be placed
#'   in parentheses following the printed percent. Defaults to \code{NULL}, in which case no additional
#'   group identifier is printed
#' @param rnd numeric vector of length == 1. The number of decimal values in the printed percent.
#'   Defaults to \code{rnd = 0}, in which case only the integer value percents will be printed.
#'   Set to \code{rnd = 1} to obtain tenths of a percent, \code{rnd = 2} to obtain hundredths, etc.
#' @param indent numeric vector of length == 1. The number of space characters to indent the printed percent.
#' @return A printed percent to the console (i/n * 100)
#' @export
#' @examples
#'  junk = sapply(1:15, function(i) {progress_updater(i, 15, rnd = 1); Sys.sleep(0.2)})

progress_updater = function(i, n, grp = NULL, rnd = 0, indent = 0) {
  # print the progress for this line
  cat("\r",
      stringr::str_pad(paste0(formatC(i/n * 100, rnd, format = "f"), "%"), 5 + rnd + indent),
      ifelse(!is.null(grp), paste0(" (", grp, ")"), ""),
      sep = ""
  )
  # print a new line if this is the last i
  if (i == n) cat("\n")
}
