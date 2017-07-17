#' Create list of bootstrapped datasets using a cluster variable
#'
#' Based on an original dataset which includes a clustering variable, creates a
#' list of \code{nboot} datasets with the cluster variable sampled with
#' replacement.
#'
#' @param df data.frame in "long" format.
#' @param cluster_var character; matches one column name in \code{df}.
#' @param nboot numeric; specifies how many datasets to create. Coerced to
#'   integer.
#' @param seed numeric; if specified, sets seed for reproducibility.
#'
#' @export
#'
#' @return A list of length \code{nboot}, where each element is a
#'   \code{data.frame} including all records from each sampled value of
#'   \code{df$cluster_var}.
#'
#' @examples
#'
#' my_df <- data.frame(
#'   id = sample(1:50, size = 500, replace = TRUE),
#'   x = rnorm(n = 500),
#'   y = rbinom(n = 500, size = 1, prob = 0.2)
#' )
#'
#' create_bootdata(df = my_df, cluster_var = "id", nboot = 25)
#'

create_bootdata <- function(df,
                            cluster_var,
                            nboot,
                            seed = NULL){

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

  return(df_list)
}
