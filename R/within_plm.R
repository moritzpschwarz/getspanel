#' Use the within transformation from the plm package
#'
#' @param df A data.frame object
#' @param effect The fixed effect specification. Values possible: "twoways" (default), "individual", "time", "nested"
#' @return A data.frame object with the transformation complete
#' @export
#'

Within_plm <- function(df, effect = "twoways"){
  new_df <- data.frame(index = 1:nrow(df))
  for(i in seq_len(ncol(df))){
    new_df <- cbind(new_df,plm::Within(df[,i],effect = effect))
    colnames(new_df)[i+1] <- eval(names(df)[i])
  }
  new_df <- new_df[-1]
  return(new_df)
}

