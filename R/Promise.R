#' @title Promise
#'
#' @description
#' \code{Promise} creates a promise that evaluates an R expression.
#'
#' @param expr the R expression to evaluate.
#' @examples
#' task1 <- Promise(rnorm(1))
#' task2 <- Promise(runif(1))
#' Execute(list(task1, task2), .combine = "c")
#' @export
Promise <- function(expr) {
  list(expr = substitute(Process(expr)),
       envir = parent.frame())
}

#' @rdname Promise
#'
#' @description
#' \code{Execute} evaluates promises and get the results.
#'
#' @param promise a list of promises to be executed
#' @param .combine function that is used to process the tasks results as they generated.
#' @param .multicombine logical flag indicating whether the \code{combine} function can accept more than two arguments.
#'   If TRUE, then the \code{combine} function will directely combine the \code{result} to final output.
#'   If FALSE, then the \code{combine} function will always be called with two arguments and reduce to final output.
#' @export
Execute <- function(promise, .combine = "list", .multicombine = TRUE) {
  Jobs <- mapply(eval,
                 expr = lapply(promise, .Primitive("[["), "expr"),
                 envir = lapply(promise, .Primitive("[["), "envir"),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  Collect(Jobs, .combine = .combine, .multicombine = .multicombine)
}
