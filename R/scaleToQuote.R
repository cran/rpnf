#' Rescales a scaled quote to original scale.
#'
#' @param x a numeric vector of scaled quotes
#' @param log TRUE or FALSE
#'
#' @return scaled quote
scaleToQuote <- function(x, log) {
  if (log==TRUE) {
    exp(x)
  } else if (log==FALSE) {
    x
  } else {
    stop("Parameter log must be TRUE or FALSE!")
  }
}
