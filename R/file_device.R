#' Create a graphics device file based on file extension
#'
#' Wouldn't it be nice to switch between (e.g.,) pdf and jpg
#' files by simply changing the file extension?
#'
#' @param file the name of the output file. Can be a path,
#'   but must contain a file extension at the end (e.g., \code{"path/to/dir/file.png"})
#' @param height the height of the device, in inches. Defaults to 5
#' @param width the width of the device, in inches. Defaults to 5
#' @param res the resolution of the device, in pixels per inch. Defaults to 600
#' @details Accepted file extentions include:
#' \itemize{
#'   \item \code{".pdf"}
#'   \item \code{".png"}
#'   \item \code{".bmp"}
#'   \item \code{".jpg"} or \code{".jpeg"}
#'   \item \code{".tif"} or \code{".tiff"}
#' }
#' @export
#' @seealso \code{\link[grDevices]{pdf}}, \code{\link[grDevices]{jpeg}},
#'   \code{\link[grDevices]{png}}, \code{\link[grDevices]{tiff}}, \code{\link[grDevices]{bmp}}
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

file_device = function(file, width = 5, height = 5, res = 600) {
  # extract the file extension
  file_type = stringr::str_remove(
    stringr::str_extract(basename(file), "\\.([[:alnum:]]+)$"),
    "\\."
  )

  # stop if no extension found
  if (is.na(file_type)) {
    stop ("No file extension found in the supplied file")
  }

  # check to ensure the file extention will match up with an accepted plotting function
  accepted_formats = c("jpeg", "jpg", "png", "pdf", "tiff", "tif", "bmp")
  if (file_type %!in% accepted_formats) {
    stop ("\n  File extension '", file_type, "' does not correspond to an accepted graphics device.\n  Valid file extentions include:",
          StatonMisc::list_out(accepted_formats, final = "or", wrap = "'"))
  }

  # obtain the name of the base graphics function to use
  # if jpg or tif were the extensions, change these to the function name
  fun_name = switch(
    file_type,
    jpg = "jpeg",
    tif = "tiff",
    file_type
  )

  # if using pdf device, change res to be 1 (pdf devices don't use resolution arguments)
  if (fun_name == "pdf") res = 1

  # build the first part of the function text
  first = paste0(fun_name, "('", file, "'")

  # build the dimension arguments
  h_arg = paste0("height = ", height, " * res")
  w_arg = paste0("width = ", width, " * res")

  # build the last part of the function text (include res if not pdf)
  if (fun_name != "pdf") last = paste0("res = ", res, ")") else last = ")"

  # build the text to parse
  parse_me = paste(first, h_arg, w_arg, last, sep = ", ")

  # evaluate the parsed text
  eval(parse(text = parse_me))
}
