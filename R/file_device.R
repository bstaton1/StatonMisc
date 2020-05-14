#' Create a graphics device file based on supplied file extension
#'
#' General purpose graphics devices saved to files, where the file type is determined by the file name
#'
#' @param filename the name of the output file. Can be a path
#'   but must contain a file extension at the end (e.g., \code{"path/to/dir/file.png"})
#' @param width the width of the device, in inches. Defaults to 5
#' @param height the height of the device, in inches. Defaults to 5
#' @param res the resolution of the device, in pixels per inch. Defaults to 600. Has no effect if the extension is \code{".pdf"} or \code{".eps"}
#' @param ... optional arguments passed to the graphics device generating function (e.g., \code{\link[grDevices]{png}})
#' @details Accepted file extensions include:
#' \itemize{
#'   \item \code{".pdf"} (calls \code{\link[grDevices]{pdf}})
#'   \item \code{".png"} (calls \code{\link[grDevices]{png}})
#'   \item \code{".bmp"} (calls \code{\link[grDevices]{bmp}})
#'   \item \code{".jpg"} (or \code{".jpeg"}, both call \code{\link[grDevices]{jpeg}}))
#'   \item \code{".tif"} (or \code{".tiff"}, both call \code{\link[grDevices]{tiff}}))
#'   \item \code{".eps"} (calls \code{\link[grDevices]{cairo_ps}})
#' }
#' @note A call to \code{\link[grDevices]{dev.off}} is still required after the plotting code (see examples).
#' @export
#' @examples
#' \dontrun{
#' # use a png device
#' file_device("test.png")
#' hist(rnorm(1000), col = "grey")
#' dev.off()
#'
#' # use a pdf device: can dump multiple plots in one file!
#' file_device("test.pdf")
#' hist(rnorm(1000), col = "grey")
#' boxplot(rnorm(1000), col = "grey")
#' dev.off()
#' }

file_device = function(filename, width = 5, height = 5, res = 600, ...) {
  # extract the file extension
  file_type = stringr::str_remove(
    stringr::str_extract(basename(filename), "\\.([[:alnum:]]+)$"),
    "\\."
  )

  # stop if no extension found
  if (is.na(file_type)) stop ("No file extension found in the supplied file")

  # check to ensure the file extention will match up with an accepted plotting function
  accepted_formats = c("jpeg", "jpg", "png", "pdf", "tiff", "tif", "bmp", "eps")
  if (file_type %!in% accepted_formats) {
    stop ("\n  File extension '", file_type, "' does not correspond to an accepted graphics device.\n  Valid file extentions include:",
          list_out(accepted_formats, final = "or", wrap = "'"))
  }

  # obtain the name of the base graphics function to use
  # if jpg, tif, or eps were the extensions, change these to the appropriate function name
  fun_name = switch(
    file_type,
    jpg = "jpeg",
    tif = "tiff",
    eps = "cairo_ps",
    file_type
  )

  # which devices take a res argument?
  res_devs = accepted_formats[accepted_formats %!in% c("pdf", "eps")]

  # if not using a res device, change res to be 1
  if (fun_name %!in% res_devs) res = 1

  # build the args list
  args_list = list(
    filename = filename,
    height = height * res,
    width = width * res
  )

  # change filename argument to file argument if using pdf output
  if (file_type == "pdf") {
    names(args_list)[1] = "file"
  }

  # add res to args if necessary
  if (file_type %in% res_devs) args_list = append(args_list, list(res = res))

  # add dot arguments if necessary
  if (...length() > 0) args_list = append(args_list, list(...))

  # call the appropriate graphics device function with the appropriate arguments
  do.call(fun_name, args_list)
}
