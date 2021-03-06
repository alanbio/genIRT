% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim2pldat.R
\name{sim2pldat}
\alias{sim2pldat}
\title{Random Dichotomous 2PL Item Data Generator}
\usage{
sim2pldat(
  nitem,
  ilocation = NULL,
  islope = NULL,
  plocation = NULL,
  nobs = 0,
  rnum = 0.5
)
}
\arguments{
\item{nitem}{The number of items}

\item{ilocation}{A data matrix of item difficulty parameter specified by user. Default is to generate nitem difficulty parameters from a standard normal distribution with a mean of 0 and sd of 1}

\item{islope}{A data matrix of item slope parameter specified by user. Default is to generate nitem slope parameters from a standard normal distribution with a mean of 0 and sd of 1}

\item{plocation}{A data matrix of person location parameter specified by user when nobs=0.}

\item{nobs}{The number of observations or rows in the person location matrix which will be generated from a standard normal distribution with a mean of 0 and sd of 1}

\item{rnum}{The random number between 0 and 1 to assign the responses. For example, when rnum=0.5, if a person's probability of responding items correctly is less than rnum, 1s will be assigned to the items. If larger than rnum, 0s will be assigned.}
}
\value{
A list
}
\description{
Random Dichotomous 2PL Item Data Generator
}
\examples{
sim2pldat(10,ilocation=NULL,islope=NULL,plocation=rnorm(500,0,1),rnum=0.6)
\dontrun{
sim2pldat(20,nobs=50,rnum=0.6)
}
}
