#' @title Process
#'
#' @description
#' \code{Process} starts a parallel R process which evaluates the given expression.
#'
#' @param expr the R expression to evaluate.
#' @examples
#' task1 <- Process(rnorm(1))
#' task2 <- Process(runif(1))
#' Collect(list(task1, task2), .combine = "c")
#' @export
Process <- function(expr) {
  parallel::mcparallel(substitute(expr, env = parent.frame()), name = NULL,
                       mc.set.seed = TRUE, silent = FALSE,
                       mc.affinity = NULL, mc.interactive = FALSE,
                       detached = FALSE)
}

#' @rdname Process
#'
#' @description
#' \code{Collect} collects results from one or more parallel processes.
#'
#' @param process a list of processes to be collected.
#' @param .combine function that is used to process the tasks results as they generated.
#' @param .multicombine logical flag indicating whether the \code{combine} function can accept more than two arguments.
#'   If TRUE, then the \code{combine} function will directely combine the \code{result} to final output.
#'   If FALSE, then the \code{combine} function will always be called with two arguments and reduce to final output.
#' @export
Collect <- function(process, .combine = "list", .multicombine = TRUE) {
  result <- parallel::mccollect(jobs = process,
                                wait = TRUE, timeout = 0,
                                intermediate = FALSE)
  Combine_Reduce(.combine, .multicombine, result)
}
