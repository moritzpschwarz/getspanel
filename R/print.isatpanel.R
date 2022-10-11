#' Printing isatpanel results
#'
#' @param x An isatpanel object.
#' @param ... Further arguments passed to print
#'
#' @return Print output of the 'isatpanel.result' list element of the 'isatpanel' object.
#' @export
#'
#'
print.isatpanel <- function(x, ...){
  gets::print.isat(x$isatpanel.result)
}
