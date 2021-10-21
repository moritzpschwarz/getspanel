#' Printing isatpanel results
#'
#' @param obj An isatpanel object.
#'#' @export
#'
#'
print.isatpanel <- function(obj){
  gets::print.isat(obj$isatpanel.result)
}
