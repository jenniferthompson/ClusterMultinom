#' Fit multinomial model and extract summary info on a list of data objects
#'
#' This function calls \code{fit_extract_multinom()} (which calls
#' \code{fit_multinom()} + \code{extract_multinom_info}) on a list of data
#' objects (\code{data.frame} or \code{\link[mice]{mice}}), then summarizes the
#' results of each.
#'
#' @param formula \code{formula}; see \code{\link[stats]{formula}} for more
#'   details.
#' @param df_list \code{list} of either \code{data.frame} or \code{mice::mids}
#'   objects on which to run model.
#' @param ref_level numeric (representing factor level) or character; level of
#'   outcome variable to use as reference.
#' @param orgdf *(optional)* \code{data.frame} or \code{mice::mids} object to
#'   "test" the model. If the model does not converge successfully with this
#'   data, the function will stop. (Example: If \code{df_list} is a list of
#'   bootstrapped data frames, \code{orgdf} would likely be the data frame from
#'   which the bootstraps come.)
#' @param orginfo *(optional)*, if \code{orgdf} supplied; one of \code{coefs}
#'   (default), \code{nothing}, or \code{modobj}. If \code{coefs}, returns only
#'   a named vector of coefficients. If \code{modobj}, returns entire model fit
#'   on \code{orgdf}. If \code{nothing}, returns nothing; only uses \code{orgdf}
#'   to test whether model runs successfully.
#' @param nsuccfits *(optional)* \code{numeric}; number of successful fits
#'   requested. If specified, and more than \code{nsuccfits} successful model
#'   fits result from \code{df_list}, only the first \code{nsuccfits} will be
#'   returned in the final function information. If specified and fewer than
#'   \code{nsuccfits} are available, all information will be returned and a
#'   warning message will display.
#' @param ... Additional arguments to pass to \code{fit_extract_multinom()}.
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
#'   \item \code{fitsucc}: numeric vector including number of model fit Successes,
#'     Failures, and (if applicable) Imputation Successes/Failures
#'   \item \code{coefs}: tibble with one row per element of \code{df_list} and one
#'     column per model coefficient (no rows for unsuccessful model fits)
#'   \item \code{impcoefs}, if requested: tibble with one row per *imputation* of
#'     \code{df_list} with a successful model fit and one column per model
#'     coefficient (no rows for unsuccessful model fits)
#'   \item \code{orgcoefs}, if \code{orgdf} is supplied and
#'     \code{orginfo = "coefs"}: named vector of coefficients from model fit on
#'     starting data frame (eg, original data frame from which \code{df_list} is
#'     bootstrapped)
#'   \item \code{orgmod}, if \code{orgdf} is supplied and
#'     \code{orginfo = "modobj"}: full model fit on starting data frame (eg,
#'     original data frame from which \code{df_list} is bootstrapped)
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
#' my_dflist <- create_bootdata(my_df, cluster_var = "id", nboot = 10)
#'
#' my_mod_summary <- summarize_multinom_list(
#'   formula = y ~ x1 + x2,
#'   df_list = my_dflist,
#'   ref_level = "A",
#'   orgdf = my_df
#' )
#'
#' ## Supply original data frame as testdf; see what happens when model is too
#' ## complex
#' my_df_small <- my_df[1:100,]
#' my_dflist_small <- create_bootdata(my_df_small, cluster_var = "id", nboot = 10)
#'
#' my_mod_summary <- summarize_multinom_list(
#'   formula = y ~ rms::rcs(x1, 4) * rms::rcs(x2, 4),
#'   df_list = my_dflist_small,
#'   ref_level = "A",
#'   orgdf = my_df_small
#' )
#'
#' ## Handle missingness with multiple imputation using mice
#' ## Also try a more complicated formula that might fail to converge
#' my_dflist_mice <- create_bootdata(
#'   my_df[1:100,], cluster_var = "id", nboot = 10, impute = TRUE
#' )
#'
#' my_mod_info_mice <- summarize_multinom_list(
#'   formula = y ~ rcs(x1, 4) * rcs(x2, 4),
#'   df_list = my_dflist_mice,
#'   ref_level = "A",
#'   coef_matrix = TRUE
#' )
#'
summarize_multinom_list <- function(formula,
                                    df_list,
                                    ref_level,
                                    orgdf = NULL,
                                    orginfo = c("coefs", "nothing", "modobj"),
                                    nsuccfits = NULL,
                                    ...){
  if(!inherits(formula, "formula")){
    stop("formula must be a formula object.", call. = FALSE)
  }

  ## If specified, nsuccfits should be less than length(df_list)
  if(!is.null(nsuccfits)){
    if(nsuccfits > length(df_list)){
      stop(
        "There are not enough elements in df_list to produce nsuccfits successful fits",
        call. = FALSE
      )
    }
  }

  ## Initialize return_list
  return_list <- NULL

  ## If orgdf is supplied, run the model on it; if it fails to converge without
  ## warnings/errors, stop the entire function. Otherwise, save and return
  ## specified info.
  if(!is.null(orgdf)){
    ## If orginfo not specified, set to coefs
    orginfo <- match.arg(orginfo)

    ## Fit original model to orgdf
    orgmod <-
      eval(
        substitute(
          with(orgdf, try_vglm(formula, family = multinomial(refLevel = ref_level)))
        )
      )

    ## If model fails to converge on orgdf, stop function
    if(inherits(orgmod, "try-error")){
      stop(
        "Specified model does not converge on test data. Suggestions to make model more stable include a less complex model and combining sparse outcome levels.",
        call. = FALSE
      )
    ## If model converges, add any info requested
    } else{
      ## Coefficients only
      if(orginfo == "coefs"){
        if(inherits(orgmod, "vglm")){
          orgcoefs <- orgmod@coefficients
          names(orgcoefs) <- reformat_vnames(names(orgcoefs))
        }
        return_list <- list("orgcoefs" = orgcoefs)
      ## Full model object
      } else if(orginfo == "modobj"){
        return_list <- list("orgmod" = orgmod)
      }
    }
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

  ## If number of requested successful fits is specified, restrict everything to
  ## as many fits as it took to reach that number
  if(!is.null(nsuccfits)){
    results_df$succ_sofar <- cumsum(results_df$fitsucc)
    results_df <-
      subset(results_df, succ_sofar <= nsuccfits, select = -succ_sofar)
  }

  ## What is the last element needed to get nsuccfits successful ones?
  ## (If nsuccfits not specified, keep everything)
  last_succ <- nrow(results_df)
  results_list <- results_list[1:last_succ]

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

  return_list <- c(
    return_list,
    list(
      "allresults" = results_df,
      "fitsuccess" = fitsuccess,
      "coefs" = coefs
    )
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

  if(!is.null(nsuccfits)){
    if(nsuccfits > nsucc){
      warning(
        sprintf(
          "*Note* that the number of successful model fits is lower than the %s specified",
          nsuccfits
        ),
        call. = FALSE
      )
    }
  }

  return(return_list)
}

#' @rdname summarize_multinom_list
#' @export
summarise_multinom_list <- summarize_multinom_list