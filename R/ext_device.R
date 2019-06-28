#' Create a new external plotting device
#'
#' A platform-flexible function for creating a new plotting device.
#' It functions just like \code{windows()} on Windows OS, \code{quartz()} on Mac OS,
#' and \code{x11()} on Linux OS.
#'
#' @param h the height, in inches
#' @param w the width, in inches
#' @param title the desired title of the plotting window
#'
#' @export

ext_device = function(h = 7, w = 7, title = NULL) {
  # determine the operating system
  if (.Platform$OS.type == "windows") {
    os = "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    os = "mac"
  } else if (.Platform$OS.type == "unix") {
    os = "unix"
  } else {
    stop("Unknown OS; cannot select the appropriate graphics device")
  }

  if (os == "win") windows(h = h, w = w, title = title)
  if (os == "mac") quartz(h = h, w = w, title = title)
  if (os == "unix") x11(h = h, w = w, title = title)
}
