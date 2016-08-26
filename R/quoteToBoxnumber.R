#' Determines the boxnumber for a given tuple of quotes,status,boxsize and log.
#'
#' @param quote a numeric vector of quotes
#' @param status current status, either "X" or "O"
#' @param boxsize boxsize
#' @param log use log scale, either TRUE or FALSE
#'
#' @return a vector of integer boxnumbers
quoteToBoxnumber <- function(quote, status, boxsize, log) {
  if (status == "X") {
    return(floor(quoteToScale(quote,log)/boxsize))
  } else if (status == "O") {
    return(ceiling(quoteToScale(quote,log)/boxsize))
  } else {
    stop(paste0("Unknown X/O-status ",status," observed!"))
  }
}
