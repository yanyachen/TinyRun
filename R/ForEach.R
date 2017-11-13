#' @title ForEach
#'
#' @description
#' Binary operator %run% that operates on a ForEach object and an R expression.
#'
#' @param ... one argument that control how expression is evaluated.
#'   Named arguments specify the name and values of variables to be defined in the evaluation environment.
#' @param .combine function that is used to process the tasks results as they generated.
#' @param .multicombine logical flag indicating whether the \code{combine} function can accept more than two arguments.
#'   If TRUE, then the \code{combine} function will directely combine the \code{result} to final output.
#'   If FALSE, then the \code{combine} function will always be called with two arguments and reduce to final output.
#' @param cores The number of cores to use.
#' @examples
#' ForEach(i = 1:5, .combine = "c", .multicombine = TRUE) %run% {
#'   rnorm(1)
#' }
#' ForEach(i = 1:5, .combine = "union", .multicombine = FALSE) %run% {
#'   round(runif(1), digits = 1)
#' }
#' @export
ForEach <- function(..., .combine = "list", .multicombine = TRUE, cores = 1L) {
  it <- list(...)
  if (length(it) > 1L) {
    stop("Only Support one argument to be iterated over")
  }
  list(arg = it[[1]],
       argname = names(it),
       combine = .combine,
       multicombine = .multicombine,
       cores = as.integer(cores))
}

#' @rdname ForEach
#'
#' @param iter ForEach object.
#' @param expr the R expression to evaluate.
#' @export
`%run%` <- function(iter, expr) {
  if (iter$cores == 1L) {
    result <- runSEQ(iter, substitute(expr), envir = parent.frame())
  } else if (iter$cores > 1L) {
    result <- runPAR(iter, substitute(expr), envir = parent.frame())
  }
  Combine_Reduce(iter$combine, iter$multicombine, result)
}

runSEQ <- function(iter, expr, envir) {
  result <- vector(mode = "list", length = length(iter$arg))
  for (i in seq_along(result)) {
    assign(iter$argname, iter$arg[i], pos = envir, inherits = FALSE)
    result[[i]] <- eval(expr, envir = envir)
  }
  return(result)
}

runPAR <- function(iter, expr, envir) {
  argList <- Map(function(x) structure(list(x), names = iter$argname),
                 iter$arg)
  FUN <- function(args) eval(expr, envir = args, enclos = envir)
  result <- parallel::mclapply(argList, FUN, mc.prePromise = TRUE,
                               mc.set.seed = TRUE, mc.silent = FALSE,
                               mc.cores = iter$cores,
                               mc.cleanup = TRUE, mc.allow.recursive = FALSE)
  return(result)
}
