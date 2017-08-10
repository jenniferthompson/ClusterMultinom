#' Run multinomial logistic regression model on a single object
#'
#' This function runs a multinomial logistic regression model on a data.frame or
#' `mice::mids` object, currently using
#' \code{VGAM::vglm(..., family = multinomial(...))}. The user can specify
#' whether to save the entire \code{vglm()} object or only the coefficients. If
#' the model fails to converge (warning or error), the warning/error message is
#' returned instead.
#'
#' @param df \code{data.frame} or \code{mice::mids} object on which to run model.
#' @param formula \code{formula}; see \code{\link[stats]{formula}} for more
#'   details.
#' @param ref_level numeric (representing factor level) or character; level of
#'   outcome variable to use as reference.
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
#' @seealso \link[VGAM]{vglm}; \link[VGAM]{multinomial}; \link[mice]{mice}
#'
#' @export
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
#'   \item \code{impcoefs}: MxP matrix of coefficients from each individual imputation
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
#' run_multinom(df = my_df, formula = y ~ x1 + x2, ref_level = "A")
#'
#' ## Handle missingness with multiple imputation using mice
#' ## Also try a more complicated formula that might fail to converge
#' my_df_mice <- mice(my_df)
#' run_multinom(
#'   df = my_df_mice,
#'   formula = y ~ rcs(x1, 5) * rcs(x2, 5),
#'   ref_level = "A",
#'   coef_matrix = TRUE
#' )
#'

run_multinom <- function(df, formula, ref_level, coef_only, ...){
  UseMethod("run_multinom", df)
}

#' describeIn run_multinom Method for data.frames
#'
#' @export
#'
run_multinom.default <- function(df,
                                 formula,
                                 ref_level,
                                 coef_only = TRUE){

  if(!inherits(formula, "formula")){
    stop("formula must be able to be coerced to a formula", call. = FALSE)
  }

  if(!inherits(df, "data.frame")){
    stop("df must be a data.frame", call. = FALSE)
  }

  ## Fit multinomial model using specified formula, reference level
  ## eval_formula_with is internal function; mice's "with(mids, ...)" syntax
  ##   breaks if formula is passed as a formula (vs character string)
  modobj <- do.call(eval_formula_with,
                    list(df = df,
                         formula = formula,
                         family = multinomial(refLevel = ref_level)))

  ## Initialize 1) vector of coefficients, 2) vector of errors/warnings
  modcoefs <- modmsgs <- NULL

  ## Was model successfully fit, without errors/warnings?
  fitsucc <- !inherits(modobj, "try-error")

  ## If model was fit with no errors or warnings, extract model coefficients
  if(fitsucc){
    modcoefs <- modobj@coefficients
  ## If model had an error or warning, return those as modmsgs
  } else{
    modmsgs <- as.character(modobj)
  }

  ## Return full model object only if requested
  if(coef_only){
    modobj <- NULL
  }

  return(list("fitsucc" = fitsucc,
              "coefs" = modcoefs,
              "msgs" = modmsgs,
              "modobj" = modobj))

}

#' describeIn run_multinom Method for \code{mice::mids} objects
#'
#' @export

run_multinom.mids <- function(df,
                              formula,
                              ref_level,
                              coef_only = TRUE,
                              coef_matrix = FALSE,
                              fail_pct = 0.8){

  if(!inherits(formula, "formula")){
    stop("formula must be able to be coerced to a formula", call. = FALSE)
  }

  if(!(fail_pct >= 0 & fail_pct <= 1)){
    stop("fail_pct must be a proportion, between 0 and 1", call. = FALSE)
  }

  ## Fit multinomial model using specified formula, reference level
  ## eval_formula_with is internal function; mice's "with(mids, ...)" syntax
  ##   breaks if formula is passed as a formula (vs character string)
  modobj <- do.call(eval_formula_with,
                    list(data = df,
                         formula = formula,
                         family = multinomial(refLevel = ref_level)))

  ## How many of the imputed models successfully converged?
  ## modconverge = vector of indicators
  modconverge <- purrr::map_lgl(modobj$analyses, ~ !inherits(., "try-error"))
  nsucc <- sum(modconverge)
  nfail <- sum(!modconverge)

  ## Initialize 1) vector of coefficients, 2) list of errors/warnings
  modcoefs <- modmsgs <- impcoefs <- NULL

  ## vglm() returns S4 objects and there is no coef() method; therefore we can't
  ##   use mice::pool(). Average point estimates from each fit manually, if at
  ##   least fail_pct of the imputations successfully fit.

  ## Indicator for whether model coefficients can be calculated
  fitsucc <- nfail < ((nsucc + nfail) * fail_pct)

  if(fitsucc){
    ## Get number of coefficients from first model that was successfully fit
    ncoefs <- length(head(modobj$analyses[modconverge], n = 1)[[1]]@coefficients)

    ## For each imputation, return either vector of NAs if model had errors/
    ##   warnings, or named vector of coefficients if it was fit successfully
    ## (extract_coefs_s4 is an internal function)
    impcoefs <- do.call(rbind,
                        map(modobj$analyses, extract_coefs_s4, ncoefs = ncoefs))

    ## Final coefficients = average of all imputations
    modcoefs <- rowMeans(impcoefs, na.rm = TRUE)
  }

  ## Get a list of all errors/warnings from model fits
  if(nfail > 0){
    modmsgs <- map_chr(modobj$analyses[!modconverge], as.character)
  }

  ## Return full mira object, matrix of all estimates only if requested
  if(coef_only){
    modobj <- NULL
  }

  if(!coef_matrix){
    impcoefs <- NULL
  }

  return(list("fitsucc" = fitsucc,
              "nfailsucc" = c("nfail" = nfail, "nsucc" = nsucc),
              "coefs" = modcoefs,
              "impcoefs" = impcoefs,
              "msgs" = modmsgs,
              "modobj" = modobj))

}

