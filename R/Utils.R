#' @title List Combining and Reducing
#'
#' @description
#' List Combining and Reducing
#'
#' @param combine function that is used to process the tasks results as they generated.
#' @param multicombine logical flag indicating whether the \code{combine} function can accept more than two arguments.
#'   If TRUE, then the \code{combine} function will directely combine the \code{result} to final output.
#'   If FALSE, then the \code{combine} function will always be called with two arguments and reduce to final output.
#' @param result list to be processed.
#' @return combined or reduced result
#' @keywords internal
Combine_Reduce <- function(combine, multicombine, result) {
  if (multicombine) {
    do.call(what = combine, args = result)
  } else {
    Reduce(f = combine, x = result, init = NULL)
  }
}
