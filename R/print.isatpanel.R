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


  if(inherits(x$isatpanel.result, "isat")){
    gets::print.isat(x$isatpanel.result)
  }

  if(inherits(x$isatpanel.result, "arx")){
    gets::print.arx(x$isatpanel.result)
  }
}
