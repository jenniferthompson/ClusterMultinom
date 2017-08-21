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
#' @param fitter Modeling function. Defaults to \code{\link{try_vglm}}.
#'
eval_formula_with <- function(data, formula, fitter = try_vglm, ...) {
  eval(substitute(with(data, fitter(formula, ...))))
}

#' Reformat a vector of strings (eg, coefficient names from VGAM::vglm()) to
#' conform with tibble naming conventions
#'
#' Conventions:
#'
#' - Coefficients for each outcome level designated by \code{_x}, where \code{x}
#' indicates the outcome level (eg, \code{x1_1, x1_2})
#' - Intercept terms designated by \code{_by_} (replaces \code{":"})
#' - Nonlinear terms designated by \code{"."} (eg, \code{x1_1, x1._1})
#'
#' @param vnames character vector
#'

reformat_vnames <- function(vnames){
  if(!inherits(vnames, "character")){
    stop("vnames must be a character vector.", call. = FALSE)
  }

  ## Step 1: substitute outcome level indicator : with _
  vnames_step1 <- gsub(":(?=[0-9]+$)", "_", vnames, perl = TRUE)

  ## Step 2: Replace "(Intercept)" with "intercept"
  vnames_step2 <- gsub("^\\(Intercept\\)", "intercept", vnames_step1)

  ## Step 3: strip "xxx(____)" and substitute interaction : with _by_
  vnames_step3 <- purrr::map_chr(
    vnames_step2,
    ~ purrr::map(strsplit(., ":")[[1]],
                 ~ gsub("^[a-z]+\\([^)]+\\)", "", .)) %>%
      paste(collapse = "_by_")
  )

  ## Step 4: replace "'" with "."
  vnames_step4 <- gsub("'", ".", vnames_step3, fixed = TRUE)

  vnames_step4
}