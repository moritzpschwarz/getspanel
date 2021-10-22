#' Printing isatpanel results
#'
#' @param x An isatpanel object.
#' @param ... Further arguments passed to print
#' @export
#'
#'
print.isatpanel <- function(x, ...){
  gets::print.isat(x$isatpanel.result)
}
