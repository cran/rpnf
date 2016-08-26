#' Returns the lower bound value for a given boxnumber
#' @param boxnumber An integer boxnumber 
#' @param boxsize single numeric value, used as the boxsize
#' @param log TRUE, if logarithmic scales should be used
box2lower <- function(boxnumber, boxsize=1, log=FALSE) {
  if(sum(floor(boxnumber)!=boxnumber,na.rm=T)>0)
    stop("Error: Only integer values allowed as boxnumber!")
  scaleToQuote(boxnumber*boxsize,log = log)
}
