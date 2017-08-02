#' Create list of cluster bootstrapped datasets
#'
#' Based on an original dataset which includes a clustering variable, this
#' function creates a list of \code{nboot} bootstrapped datasets with the
#' cluster variable (as opposed to rows) sampled with replacement. If using
#' complete case analysis, the function returns that list; if using multiple
#' imputation, the function returns a list of \code{nboot} \code{mice::mids}
#' objects based on each of those bootstrapped datasets.
#'
#' @param df data.frame in "long" format.
#' @param cluster_var character; matches one column name in \code{df}.
#' @param nboot numeric; specifies how many datasets to create. Coerced to
#'   integer.
#' @param seed numeric; if specified, sets seed for reproducibility.
#' @param impute logical; whether to use \code{mice} to impute each bootstrapped
#'   dataset.
#' @param ... Additional arguments to pass to \code{mice} if desired. See
#' \code{\link[mice]{mice}} for default settings and additional information.
#'
#' @seealso \link[mice]{mice}
#'
#' @export
#'
#' @return A list of length \code{nboot}, where each element is either 1) a
#'   \code{data.frame} including all records from each sampled value of
#'   \code{df$cluster_var}, or 2) a \code{mice} object based on one of the
#'   bootstrapped datasets.
#'
#' @examples
#'
#' my_df <- data.frame(
#'   id = sample(1:50, size = 500, replace = TRUE),
#'   x = rnorm(n = 500),
#'   y = rbinom(n = 500, size = 1, prob = 0.2)
#' )
#'
#' ## Without imputation:
#' create_bootdata(df = my_df, cluster_var = "id", nboot = 25)
#'
#' ## To demo imputation, make some data 10% MCAR
#' my_df_mcar <- my_df
#' my_df_mcar$x[sample(1:500, size = 50)] <- NA
#' my_df_mcar$y[sample(1:500, size = 50)] <- NA
#'
#' ## Use default arguments to mice()
#' create_bootdata(
#'   df = my_df_mcar,
#'   cluster_var = "id",
#'   nboot = 25,
#'   impute = TRUE
#' )
#'
#' ## Supply arguments to mice()
#' create_bootdata(
#'   df = my_df_mcar,
#'   cluster_var = "id",
#'   nboot = 25,
#'   impute = TRUE,
#'   m = 10,
#'   method = "mean"
#' )

create_bootdata <- function(df,
                            cluster_var,
                            nboot,
                            seed = NULL,
                            impute = FALSE,
                            ...){

  ## -- Make sure arguments are as expected ------------------------------------
  if(!inherits(df, "data.frame")){
    stop("df must be a data.frame", call. = FALSE)
  }

  if(!inherits(cluster_var, "character")){
    stop("cluster_var must be a character string", call. = FALSE)
  }

  ## Make sure cluster_var is a column in df
  if(!(cluster_var %in% names(df))){
    stop("cluster_var must be a column in df", call. = FALSE)
  }

  if(!inherits(nboot, "numeric") | nboot < 1){
    stop("nboot must be numeric value >= 1", call. = FALSE)
  }

  if(!is.null(seed)){
    if(!inherits(seed, "numeric")){
      stop("seed must be numeric", call. = FALSE)
    }
  }

  ## -- Generate vector of seeds of length = nboot, using seed if specified ----
  if(!is.null(seed)){
    set.seed(seed)
  }
  boot_seeds <- sample(0:(nboot * 1000), size = nboot)

  ## -- Randomly sample cluster_var with replacement nboot times ---------------
  ## Size = length(unique(df$cluster_var))
  unique_clusters <- unique(df[ , cluster_var])

  ## Get a list of cluster samples given boot_seeds
  cluster_samples <- purrr::map(boot_seeds, sample_clusters, unique_clusters)

  ## -- For each sample of clusters: -------------------------------------------
  ## 1. Get all records for each cluster ID
  ## 2. Concatenate
  ## Do this for all samples, then return in a list
  df_list <- purrr::map(cluster_samples,
                        ~ purrr::map_df(., ~ df[df[,cluster_var] == .,]))

  ## -- If imputation is specified: --------------------------------------------
  ## For each element of df_list, run mice(), using any user-supplied arguments
  ## Return list of mids objects (length = nboot)
  if(impute){
    if(requireNamespace("mice", quietly = TRUE)){

      df_list_imp <- purrr::map(df_list, ~ mice(., ...))

      return(df_list_imp)

    } else{
      stop(
        "You must have the mice package installed in order to use multiple imputation.",
        call. = FALSE
      )
    }

  ## -- If no imputation specified, return original list of bootstrapped dfs ---
  } else{
    return(df_list)
  }

}
