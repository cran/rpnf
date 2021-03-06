% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnfplot.R
\name{pnfplot}
\alias{pnfplot}
\title{Generate a modern point and figure plot}
\usage{
pnfplot(data, reversal = 3, boxsize = 1, log = FALSE, ...)
}
\arguments{
\item{data}{a data frame object containing point and figure informations to be plotted}

\item{reversal}{number of boxes used in pnfprocessor}

\item{boxsize}{the boxsize used in pnfprocessor}

\item{log}{are calculations done in logarithmic mode}

\item{...}{any additional options for the plot command}
}
\description{
Generate a modern point and figure plot
}
\examples{
library(rpnf) # Load rpnf library
data(DOW) # (Offline) Load free available sample data from https://www.quandl.com/data/WIKI/DOW
pnfdata <- pnfprocessor(
  high=DOW$High,
  low=DOW$Low,
  date=DOW$Date,
  boxsize=1L,
  log=FALSE)  
pnfplot(pnfdata,boxsize=1L,log=FALSE)
}
\references{
\url{http://rpnf.r-forge.r-project.org}
}
\seealso{
\code{\link{pnfprocessor}}

\code{\link{pnfplottxt}}
}

