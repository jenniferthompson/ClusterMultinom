#' Return a sample of a vector with replacement, given a seed
#'
#' @param use_seed numeric; seed to set
#' @param use_clusters vector to sample from
#'
sample_clusters <- function(use_seed, use_clusters){
  set.seed(use_seed)
  sample(use_clusters, size = length(use_clusters), replace = TRUE)
}

#' Extract coefficients from an S4 model object that may be of class try-error
#'
#' @param x either S4 model object or object of type try-error
#' @param ncoefs numeric; number of missing values to return if object is try-error
#'
extract_coefs_s4 <- function(x, ncoefs){
  if(inherits(x, "try-error")){
    rep(NA, ncoefs)
  } else{
    x@coefficients
  }
}

#' Wrapper to allow formula object to be passed to with([mids object], vglm(...))
#'
#' Passing a formula object to a with(mids object, ...) call breaks.
#' eval(substitute()) makes it work.
#'
#' @param data Object on which you can fit a model of type \code{fitter}. Most
#'   cases, a \code{data.frame} or \code{mids} object.
#' @param formula \code{formula}; see \code{\link[stats]{formula}} for more
#'   details.
#' @param fitter Modeling function. In this package, defaults to
#'   \code{\link{try.vglm}}.
#'
eval_formula_with <- function(data, formula, fitter = try_vglm, ...) {
  eval(substitute(with(data, fitter(formula, ...))))
}