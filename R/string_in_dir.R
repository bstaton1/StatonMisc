#' Find string occurrences in files within a directory
#'
#' Finds the location, frequency, and context with which a given string is used
#' in files contained within a specified directory. Regular expressions are
#' supported to facilitate searching in both string location and file selection.
#'
#' @param path a character string (length == 1) pointing to the location of the directory to search.
#'   Passed to \code{dir(path)}. Defaults to \code{NULL}, which specifies the working directory
#' @param string a character string (length == 1) indicating the string to search for
#' Regular expressions are supported here
#' @param print_negs logical. Do you wish to see the names of the files were searched but
#'   that do not have any matches? Defaults to \code{FALSE}
#' @param print_lines logical or \code{"ask"}. Do you wish to see the specific line matches in each file?
#'   Specifying \code{"ask"} will ask the user before printing. Defaults to \code{FALSE}
#' @param ... optional arguments to be passed to \code{dir()} when selecting files to search.
#'   For example, \code{recursive = TRUE} will search files within subdirectories,
#'   and \code{pattern = ".R|.Rmd"} will search only within files with these extensions.
#'
#' @return The search results are printed to the console.
#'
#' @seealso \code{\link[base]{dir}}
#'
#' @export

# root = "C:/Users/bstaton/Desktop/Staton/2_kusko/manuscripts/mixed-stockSRA/"
# strings_in_dir(path = root, string = "^title", recursive = T, print_lines = T)

string_in_dir = function(path = NULL, string, print_negs = F, print_lines = F, ...) {

  # use wd if path is not specified
  if (is.null(path)) path = getwd()

  # check to see if print_lines was correctly specified
  if (print_lines != "ask" & !is.logical(print_lines)) {
    stop("'print_lines' must be logical or 'ask'")
  }

  # handle triggers for asking about printing lines
  if (print_lines == "ask") {
    print_lines = F
    ask = T
  } else ask = F

  # find the files to search for.
    # example "..." options are:
      # recursive = T; for searching within subdirectories
      # pattern = ".R|.Rmd"; for searching only within these extention types
  files = dir(path, ...)

  # set a counter for negative matches
  neg_counter = 0

  # loop through files i
  for (i in 1:length(files)) {

    # read the contents of this file (if possible)
    contents = readLines(file.path(path, files[i]), warn = F)
    matches = stringr::str_detect(contents, string)
    if(any(matches)) {
      cat("!!!!!", files[i], " on ", sum(matches), " line(s)\n", sep = "")
      if (ask) answer = readline(prompt = "Print matches? (y/n):") else answer = NA
      if (answer %in% c("y", "Y") | print_lines) {
        sapply(which(matches), function(m) {
          cat("  -> line ", m, ": ", stringr::str_trunc(contents[m], width = 80), "\n", sep = "")
        })
      }
    } else {
      neg_counter = neg_counter + 1
      if (print_negs) cat("no matches in file ", files[i], "\n", sep = "")
    }
  }
  if (!print_negs) cat("\n", neg_counter, "file(s) searched with no matches found")
}
