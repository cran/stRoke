#' @title Source Lines from a File
#' @description Sources specific lines from a file
#'
#' @param file A character string giving the path to the file to be sourced.
#' @param lines A numeric vector of line numbers to be sourced.
#' @param ... Additional arguments to be passed to \code{\link{source}}.
#'
#' @return The result of \code{\link{source}}.
#'
#' @examples
#' test_file <- tempfile(fileext = ".R")
#' writeLines(c("# Line 1", "2+2", "# Line 3"), test_file)
#' source_lines(test_file, 1:2, echo=TRUE)
#'
#' @export
#' @seealso This function is borrowed from a
#' [gist](https://gist.github.com/christophergandrud/1eb4e095974204b12af9)
#' by christophergandrud.
#'
source_lines <- function(file, lines, ...) {
  source(textConnection(readLines(file)[lines]), ...)
}
