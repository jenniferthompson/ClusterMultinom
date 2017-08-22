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
#' @return List containing the following elements:
#' \itemize{
#'   \item \code{allresults}: \code{\link{[tibble]tibble}} containing one row
#'     per element of \code{df_list}, with the following information for each
#'     included as variables or [list columns](https://www.rstudio.com/resources/videos/using-list-cols-in-your-dataframe/):
#'     \itemize{
#'       \item \code{fitsucc}: logical; indicates whether model was successfully
#'         fit to that data object
#'       \item \code{coefs}: tibble with one row, and one column per coefficient
#'       \item \code{impcoefs}, if requested: tibble with coefficient estimates
#'         from each imputation
#'       \item \code{msgs}: list of character strings; errors/warning messages
#'         from unsuccessful fits
#'       \item \code{modobj}, if requested: full model object
#'     }
#'   \item\code{fitsucc}: numeric vector including number of model fit Successes,
#'     Failures, and (if applicable) Imputation Successes/Failures
#'   \item\code{coefs}: tibble with one row per element of \code{df_list} and one
#'     column per model coefficient (plus element indicator); coefficients = NA
#'     for unsuccessful model fits
#'   \item\code{impcoefs}, if requested: tibble with one row per *imputation* of
#'     \code{df_list} and one column per model coefficient; NA for unsuccessful
#'     model fits
#' }
#'
#' @examples CHANGE THIS
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
    fitsucc = purrr::map_lgl(results_list, "fitsucc"),
    coefs = purrr::map(results_list, "coefs"),
    msgs = purrr::map(results_list, "msgs")
  )

  ## Add additional columns if requested
  if("nfailsucc" %in% names(results_list[[1]])){
    results_df$nfailsucc <- purrr::map(results_list, "nfailsucc")
  }
  if("impcoefs" %in% names(results_list[[1]])){
    results_df$impcoefs <- purrr::map(results_list, "impcoefs")
  }
  if("modobj" %in% names(results_list[[1]])){
    results_df$modobj <- purrr::map(results_list, "modobj")
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

  ## -- Coefficient point estimates --------------------------------------------
  ## If there was at least one successful model fit, create a tibble with
  ## [number of successful fits] rows * p columns. Elements of df_list
  ## for which the model did not converge are ignored.
  ## ie: If 3 out of 5 elements of df_list converged, coefs will be a tbl_df
  ## with 3 rows * p columns.
  ## If no elements of df_list were fit successfully, return NULL.
  if(nsucc > 0){
    coefs <- purrr::map_df(results_list, "coefs")
  } else{
    coefs <- NULL
  }

  return_list <- list(
    "allresults" = results_df,
    "fitsuccess" = fitsuccess,
    "coefs" = coefs
  )

  ## If requested and if elements of df_list are mice objects, return a tibble
  ## of all coefficients from each *imputation*. ie, if there are 5 elements of
  ## df_list, each imputed 5 times, and all fits were successful, this tibble
  ## will have 5*5=25 rows and p columns.
  if("impcoefs" %in% names(results_df)){
    if(nsucc > 0){
      impcoefs <- purrr::map_df(results_list, "impcoefs")
    } else{
      impcoefs <- NULL
    }
    return_list <- c(return_list, list("impcoefs" = impcoefs))
  }

  return(return_list)
}
