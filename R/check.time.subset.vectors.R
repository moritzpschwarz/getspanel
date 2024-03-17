#' Internal function to check vectors that subset the indicator selection using the time dimension
#'
#' @param time.vector A vector containing the user input in e.g. \code{tis_time} or \code{fesis_time}
#' @param vector.name The name of argument that the user inputted this vector in. This is just to make error messages more elaborate.
#' @param time The time dimension of isatpanel.
#' @param id The id dimension of isatpanel.
#'
#' @return Does not return any value but will throw error if something is not correct.
#'
check.time.subset.vectors <- function(time.vector, vector.name, time, id){
  # Check that it is the right format
  if(!is.list(time.vector) & !class(time) == class(time.vector)){
    stop(paste0("'",vector.name,"' must either be in the format of the time dimension or a list with one element per id (each element of that list must then either be in the format of the time dimension or NULL)."))
  }

  # if a list element is provided, check each element
  if(is.list(time.vector)){
    # if vector.name = "csis_time" throw immediate error
    if(vector.name == "csis_time"){stop("For 'csis_time' specifying a list is not allowed. CSIS is always looking for coefficient step shifts across all units, so 'csis_time' must specify a time period that is equal across all units (e.g. 1:10).")}

    # check first that all elements are names
    if(is.null(names(time.vector))){stop(paste0("All elements of '",vector.name,"' must be named to be able to be matched to the id's. For example: list(A = 1:10, B = NULL, C = 5:10)"))}

    if(length(time.vector) != length(unique(id))){
      stop(paste0("When providing '",vector.name,"' as a list, the number of elements must be equal to the number of id's. Each element must either be the same data format as the time column in the data or NULL. You have provided ",
                  length(time.vector)," elements and there are ",length(unique(id))," unique group id's in the data."))
    }

    if(!all(names(time.vector) %in% id)){stop(paste0("All named elements of '",vector.name,"' must be an id in the data. The following ids are not in the data: ", paste0(names(time.vector)[!names(time.vector) %in% id], collapse = ",")))}

    # check that all elements are either NULL or numeric
    if(!all(unlist(lapply(time.vector, function(x){class(time) == class(x) | is.null(x)})))){
      stop(paste0("The elements of the list '",vector.name,
                  "' must be either identical to the data format of the time column in the data or NULL. The class of the time column is ",
                  class(time)," and the class(es) of '",vector.name,"' is/are: ",paste0(unique(unlist(lapply(time.vector, class))),".",collapse = ",")))}

    # then check that all of them are in time (so that they are not out of sample) - NULL equates here to TRUE
    if(!all(unlist(lapply(time.vector, function(x){x %in% time})))){
      stop(paste0("Some or all time periods in '",vector.name,"' not found in the data. Please check the input under '",vector.name,"'. The period(s) ",
                  paste0(unique(unlist(time.vector)[!unlist(lapply(time.vector, function(x){x %in% time}))]), collapse = ",")," is/are not found in the data."))}

  }
  if (class(time) == class(time.vector)){
    if (!all(time.vector %in% time)){
      stop(paste0("Some or all time periods in '",vector.name,"' not found in the data. Please check the input under '",vector.name,"'. The period(s) ",
                  paste0(unique(time.vector[!time.vector %in% time]), collapse = ",")," is/are not found in the data."))}
  }
}

