#' Return a sample of a vector with replacement, given a seed
#'
#' @param use_seed numeric; seed to set
#' @param use_clusters vector to sample from
#'
sample_clusters <- function(use_seed, use_clusters){
  set.seed(use_seed)
  sample(use_clusters, size = length(use_clusters), replace = TRUE)
}