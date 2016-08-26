#' Determine the next box frontier for current quote(s) given a recent XO-status.
#' 
#' Note: offset should only be used for reversal calculation
#' @param quote  A single quote or a vector of quotes.
#' @param status A single character indicating the current XO-status.
#' @param boxsize A single numeric value, indicating the boxsize to be considered.
#' @param log TRUE, if logarithmic scales should be used.
nextBox <- function(quote,status, boxsize=1, log=FALSE) {
  if (!(is.numeric(quote))) {
    stop("Argument quote has to be numeric!")
  }
  if (!(is.character(status) & nchar(status)==1 )) {
    stop("Argument status has to be a character and of length 1!")
  }
  if (!(is.numeric(boxsize) & length(boxsize)==1)) {
    stop("Argument boxsize has to be numeric and of length 1!")
  }
  if(!(is.logical(log) & length(log))) {
    stop("Argument log has to be logical and of length 1!")
  }

  if (status == "X") {
    return(scaleToQuote((quoteToBoxnumber(quote,status,boxsize,log)+1)*boxsize,log))
  } else if (status == "O") {
    return(scaleToQuote((quoteToBoxnumber(quote,status,boxsize,log)-1)*boxsize,log))
  } else {
    stop(paste0("Unknown X/O-status ",status,"observed!"))
  }
}

