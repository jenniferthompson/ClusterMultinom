#' Fit multinomial model and extract summary info on a list of data objects
#'
#' This function calls \code{fit_extract_multinom()} (which calls
#' \code{fit_multinom()} + \code{extract_multinom_info}) on a list of data
#' objects (\code{data.frame} or \code{\link[mice]{mice}}) and summarizes the
#' results of each.
#'
#' @export
#'
#' @seealso \code{\link{fit_multinom}}, \code{\link{extract_multinom_info}},
#'   \code{\link{fit_extract_multinom}}. \code{\link[VGAM]{vglm}};
#'   \code{\link[VGAM]{multinomial}} for model fitting. \code{\link[mice]{mice}}
#'   for imputation.
#'
#' @return List containing the following elements CHANGE THIS:
#' \itemize{
#'   \item \code{fitsucc}: logical indicating whether the model was successfully fit,
#'     with no warnings/errors
#'   \item\code{coefs}: numeric vector of model coefficients (NULL if
#'     \code{fitsucc} = FALSE)
#'    \item\code{msgs}: character vector of warnings/error messages (NULL if
#'     \code{fitsucc} = TRUE)
#'    \item\code{modobj}: full model object of class \code{VGAM::vglm()}. Only
#'     returned if requested (\code{coef_only} = FALSE).
#' }
#'
#' If \code{df} is a \code{mice::mids} object, the following elements are also
#'   returned:
#' \itemize{
#'   \item \code{nfailsucc}: numeric vector; number of successful and failed model
#'   fits among imputations
#'   \item \code{impcoefs} (if \code{coef_matrix = TRUE}): MxP matrix of
#'     coefficients from each individual imputation
#' }
#'
#' @examples
#'
#' my_df <- data.frame(
#'   id = sample(1:50, size = 500, replace = TRUE),
#'   x1 = sample(c(NA, 1:100), size = 500, replace = TRUE),
#'   x2 = sample(c(NA, 1:100), size = 500, replace = TRUE),
#'   y = sample(c(NA, LETTERS[1:3]), size = 500, replace = TRUE)
#' )
#'
#' ## Basic usage
#' my_mod_info <- fit_extract_multinom(
#'   formula = y ~ x1 + x2,
#'   df = my_df,
#'   ref_level = "A"
#' )
#'
#' ## Handle missingness with multiple imputation using mice
#' ## Also try a more complicated formula that might fail to converge
#' my_df_mice <- mice(my_df[1:125,])
#' my_mod_info_mice <- fit_extract_multinom(
#'   formula = y ~ rcs(x1, 5) * rcs(x2, 5),
#'   df = my_df_mice,
#'   ref_level = "A",
#'   coef_matrix = TRUE
#' )
#'
summarize_multinom_list <- function(formula, df_list, ref_level, ...){
  if(!inherits(formula, "formula")){
    stop("formula must be a formula object.", call. = FALSE)
  }

  ## Fit model to each data object and extract info
  results_list <- purrr::map(df_list,
                             fit_extract_multinom,
                             formula = formula,
                             ref_level = ref_level,
                             ...)

  ## Summarize and save everything in a tidy data.frame with the following
  ## columns (* = for mice::mids objects only):
  ## - element: list element (1:length(df_list))
  ## - fitsucc: indicator of overall success
  ## - nfailsucc*: named numeric vector of failures, successes
  ## - coefs: named numeric vector of coefficient estimates
  ## - msgs: warning/error messages
  ## - impcoefs*, if requested: matrix of coefficient estimates from each
  ##     imputation (coefs = colMeans of these)
  ## - modobj, if requested: full model object

  ## Basically, we want to take every element of what's returned from
  ## fit_extract_multinom() and store it in a data.frame, one row per element
  ## of df_list.

  results_df <- tibble(
    element = 1:length(df_list),
    fitsucc = purrr::map_lgl(results_list, ~ .[["fitsucc"]]),
    coefs = purrr::map(results_list, ~ .[["coefs"]]),
    msgs = purrr::map(results_list, ~ .[["msgs"]])
  )

  ## Add additional columns if requested
  if("nfailsucc" %in% names(results_list[[1]])){
    results_df$nfailsucc <- purrr::map(results_list, ~ .[["nfailsucc"]])
  }
  if("impcoefs" %in% names(results_list[[1]])){
    results_df$impcoefs <- purrr::map(results_list, ~ .[["impcoefs"]])
  }
  if("modobj" %in% names(results_list[[1]])){
    results_df$modobj <- purrr::map(results_list, ~ .[["modobj"]])
  }

  ## Sort applicable columns in desired order
  col_order <-
    c("element", "fitsucc", "nfailsucc", "coefs", "impcoefs", "msgs", "modobj")
  results_df <-
    results_df[, names(results_df)[order(match(names(results_df), col_order))]]

  ## We want easy access to number of successes/failures, coefficient matrix,
  ## and imputation coefficients if applicable - summarize those

  ## Successes/failures (including imputations, if applicable)
  nsucc <- sum(results_df$fitsucc)
  nfail <- sum(!results_df$fitsucc)
  fitsuccess <- c("Successes" = nsucc, "Failures" = nfail)

  if("nfailsucc" %in% names(results_df)){
    nimpsucc <- sum(purrr::map_int(results_df$nfailsucc, ~ .["succ"]))
    nimpfail <- sum(purrr::map_int(results_df$nfailsucc, ~ .["fail"]))
    fitsuccess <-
      c(fitsuccess, "ImpSuccesses" = nimpsucc, "ImpFailures" = nimpfail)
  }

  ## Coefficient point estimates
  coefs <- purrr::map_df(1:length(results_list),
                         ~ data.frame(element = .,
                                      results_list[[.]][["coefs"]]))

  return_list <- list(
    "allresults" = results_df,
    "fitsuccess" = fitsuccess,
    "coefs" = coefs
  )

  if("impcoefs" %in% names(results_df)){
    impcoefs <- purrr::map_df(1:length(results_list),
                              ~ data.frame(element = .,
                                           results_list[[.]][["impcoefs"]]))
    return_list <- c(return_list, list("impcoefs" = impcoefs))
  }

  return(return_list)
}
