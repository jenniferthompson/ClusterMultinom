#' Silently catch errors for \code{vglm()} calls
#'
#' @param ... Arguments to be passed to \code{VGAM::vglm()}.
#'
#' @export

try_vglm <- function(...){
  ## Turn all warning messages into errors; reset upon exit
  op <- options(warn = 2)
  on.exit(options(op))
  ## Fit VGAM::vglm() without warnings
  try(VGAM::vglm(..., silent = TRUE))
}
