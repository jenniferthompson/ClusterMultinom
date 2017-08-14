#' Fit multinomial logistic regression model on a single object
#'
#' This function runs a multinomial logistic regression model on a data.frame or
#' `mice::mids` object, currently using \code{\link{try_vglm}} (which runs
#' \code{VGAM::vglm(..., family = multinomial(...))}, but if the model fails to
#' converge - ie, warning or error - the warning/error message is returned
#' instead).
#'
#' This wrapper function is necessary to perform this operation on
#' \code{\link[mice]{mids}} objects, which require a \code{with(..., )} syntax.
#'
#' @param formula \code{formula}; see \code{\link[stats]{formula}} for more
#'   details.
#' @param df \code{data.frame} or \code{mice::mids} object on which to run model.
#' @param ref_level numeric (representing factor level) or character; level of
#'   outcome variable to use as reference.
#' @param fitter Function to use for model fitting. Currently only option is
#'   \code{\link{try_vglm}}.
#'
#' @export
#'
#' @seealso \code{\link[VGAM]{vglm}}; \code{\link[VGAM]{multinomial}} for model
#'   fitting. \code{\link[mice]{mice}} for imputation.
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
#' ## Works whether df is a data.frame or mice() object
#' fit_multinom(y ~ x1 + x2, my_df, ref_level = "A")
#' fit_multinom(y ~ x1 + x2, mice::mice(my_df), ref_level = "A")

fit_multinom <- function(formula, df, ref_level, fitter = try_vglm){

  if(!inherits(formula, "formula")){
    stop("formula must be able to be coerced to a formula", call. = FALSE)
  }

  eval(
    substitute(
      with(df, fitter(formula, family = multinomial(refLevel = ref_level)))
    )
  )
}

#' Extract information from a single try_vglm fit
#'
#' This function extracts summary information from a model fit using
#' \code{\link{try_vglm}} and returns it in a list for easy access.
#'
#' @param modobj Object created using \code{\link{try_vglm}}, of class
#'   \code{try-error}, \code{\link[VGAM]{vglm}}, or \code{\link[mice]{mids}}.
#' @param coef_only logical; whether to return only model coefficients
#'   (recommended to save memory; in the context of bootstrapping, coefficients
#'   are often the only required information) or entire model object. Defaults
#'   to TRUE.
#' @param coef_matrix logical; whether to return a matrix of all coefficient
#'   estimates from each imputation in a \code{mids} object, in addition to the
#'   averages for each coefficient. Defaults to FALSE.
#' @param fail_pct numeric, used if \code{df} is a \code{mids} object;
#'   proportion of imputed datasets that can have errors or warnings and still
#'   be able to calculate point estimates. Defaults to 0.8 (80\% of imputations
#'   must fit with no errors or warnings).
#'
#' @export
#'
#' @seealso \code{\link[VGAM]{vglm}}; \code{\link[VGAM]{multinomial}} for model
#'   fitting. \code{\link[mice]{mice}} for imputation.
#'
#' @return List containing the following elements:
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
#' my_mod <- fit_multinom(formula = y ~ x1 + x2, df = my_df, ref_level = "A")
#' extract_multinom_info(my_mod)
#'
#' ## Handle missingness with multiple imputation using mice
#' ## Also try a more complicated formula that might fail to converge
#' my_df_mice <- mice(my_df)
#' my_mod_mice <- fit_multinom(
#'   formula = y ~ rcs(x1, 5) * rcs(x2, 5),
#'   df = my_df_mice,
#'   ref_level = "A"
#' )
#'
#' extract_multinom_info(my_mod_mice, coef_matrix = TRUE)
#'

extract_multinom_info <- function(modobj, coef_only = TRUE, ...){
  UseMethod("extract_multinom_info", modobj)
}

#' describeIn extract_multinom_info Method for models not fit with
#'   \code{\link[mice]{mids}} objects
#'
#' @export
#'
extract_multinom_info.default <- function(modobj, coef_only = TRUE){

  fitsucc <- !inherits(modobj, "try-error")

  ## If model was not fit successfully (no warnings/errors), return
  ##  fitsucc = FALSE, list of warnings/errors, and NULL for everything else
  if(!fitsucc){
    coefs <- NULL
    msgs <- as.character(modobj)

  ## If model was fit successfully, extract vector of coefficients
  } else{
    coefs <- modobj@coefficients
    msgs <- NULL
  }

  if(coef_only){
    return(list("fitsucc" = fitsucc,
                "coefs" = coefs,
                "msgs" = msgs))
  } else{
    return(list("fitsucc" = fitsucc,
                "coefs" = coefs,
                "msgs" = msgs,
                "modobj" = NULL))
  }

}

#' describeIn extract_multinom_info Method for models fit with
#'   \code{\link[mice]{mids}} objects
#'
#' @export
#'
extract_multinom_info.mira <- function(modobj,
                                       coef_only = TRUE,
                                       coef_matrix = FALSE,
                                       fail_pct = 0.8){

  if(!(fail_pct >= 0 & fail_pct <= 1)){
    stop("fail_pct must be a proportion between 0 and 1.", call. = FALSE)
  }

  ## How many imputations had successful model fits?
  nfits <- length(modobj$analyses)
  imp_fitsucc <- purrr::map_lgl(modobj$analyses, ~ !inherits(., "try-error"))

  nsucc <- sum(imp_fitsucc)
  nfail <- sum(!imp_fitsucc)

  ## Fit is considered "successful" if at least fail_pct of imputations had no
  ## errors or warnings. If fit is not successful, we do not attempt to get
  ## any further info.
  fitsucc <- round(nsucc / nfits) >= fail_pct

  ## If model was not fit successfully, return fitsucc = FALSE, list of
  ##   warnings/errors, and NULL for everything else
  if(!fitsucc){
    coefs <- impcoefs <- NULL
  } else{
    ## vglm() returns S4 objects and there is no coef() method; therefore we
    ##   can't use mice::pool(). So we average point estimates from each fit
    ##   manually.

    ## Get number of expected coefficients from first imputation that was
    ##   successfully fit
    ncoefs <- length(head(modobj$analyses[imp_fitsucc], n = 1)[[1]]@coefficients)

    ## For each imputation, return either vector of NAs if model had errors/
    ##   warnings, or named vector of coefficients if it was fit successfully
    ## (extract_coefs_s4 is an internal function)
    impcoefs <- do.call(rbind,
                        map(modobj$analyses, extract_coefs_s4, ncoefs = ncoefs))

    ## Final coefficients = average of all imputations
    coefs <- colMeans(impcoefs, na.rm = TRUE)
  }

  ## Get a list of all errors/warnings from model fits
  if(nfail > 0){
    msgs <- map_chr(modobj$analyses[!imp_fitsucc], as.character)
  } else{
    msgs <- NULL
  }

  return_list <- list("fitsucc" = fitsucc,
                      "nfailsucc" = c("fail" = nfail, "succ" = nsucc),
                      "coefs" = coefs,
                      "msgs" = msgs)

  ## If requested, add matrix of all coefficients from each imputation, full
  ## mira object

  if(coef_matrix){
    return_list <- c(return_list, list("impcoefs" = impcoefs))
  }

  if(!coef_only){
    return_list <- c(return_list, list("modobj" = modobj))
  }

  return_list

}

#' Fit multinomial model and extract summary info on a single data object
#'
#' This is a wrapper for \code{fit_multinom()} + \code{extract_multinom_info};
#' it fits a model and extracts summary information (indicator for successful
#' fit, vector of coefficients, and character vector of errors/warnings) given
#' a formula, a single data object (\code{data.frame} or \code{mice::mids}
#' object), and a reference level for the outcome.
#'
#' @export
#'
#' @seealso \code{\link{fit_multinom}}, \code{\link{extract_multinom_info}}.
#'   \code{\link[VGAM]{vglm}}; \code{\link[VGAM]{multinomial}} for model
#'   fitting. \code{\link[mice]{mice}} for imputation.
#'
#' @return List containing the following elements:
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
fit_extract_multinom <- function(formula, df, ref_level, ...){
  if(!inherits(formula, "formula")){
    stop("formula must be a formula object.", call. = FALSE)
  }

  ## Can't pass directly as formula object though... environments :disappointed:
  ## Hack a solution
  mod <- fit_multinom(
    as.formula(paste(as.character(formula)[c(2, 1, 3)], collapse = " ")),
    df,
    ref_level
  )
  modinfo <- extract_multinom_info(mod, ...)
  modinfo
}
