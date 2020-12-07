#' Printing isatpanel results
#'
#' @param obj An isatpanel object.
#'
#' @return
#' @export
#'
#'
print.isatpanel <- function(obj){
  gets::print.isat(obj$isatpanel.result)
}
