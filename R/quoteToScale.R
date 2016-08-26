#' Scales a quote. In case log==TRUE this is logarithmic scale, original scale otherwise.
#'
#' @param x a numeric vector of quotes
#' @param log TRUE or FALSE
#'
#' @return scaled quote
quoteToScale <- function(x, log) {
  if (log==TRUE) {
    log(x)
  } else if (log==FALSE) {
    x
  } else {
    stop("Parameter log must be TRUE or FALSE!")
  }
}

