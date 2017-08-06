#' Return a sample of a vector with replacement, given a seed
#'
#' @param use_seed numeric; seed to set
#' @param use_clusters vector to sample from
#'
sample_clusters <- function(use_seed, use_clusters){
  set.seed(use_seed)
  sample(use_clusters, size = length(use_clusters), replace = TRUE)
}

#' Silently catch errors for \code{vglm()} calls
#'
#' @param ... Arguments to be passed to \code{VGAM::vglm()}.
#'

try.vglm <- function(...){
  ## Turn all warning messages into errors; reset upon exit
  op <- options(warn = 2)
  on.exit(options(op))
  ## Fit vglm() without warnings
  try(vglm(..., silent = TRUE))
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
