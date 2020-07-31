#' Random Dichotomous Rasch Item Data Generator
#'
#' @param nitem The number of items
#' @param ilocation A data matrix of item difficulty parameter specified by user. Default is to generate nitem difficulty parameters from a standard normal distribution with a mean of 0 and sd of 1
#' @param plocation A data matrix of person location parameter specified by user when nobs=0.
#' @param nobs The number of observations or rows in the person location matrix which will be generated from a standard normal distribution with a mean of 0 and sd of 1
#' @param rnum The random number between 0 and 1 to assign the responses. For example, when rnum=0.5, if a person's probability of responding items correctly is less than rnum, 1s will be assigned to the items. If larger than rnum, 0s will be assigned.
#' @import stats
#' @return A list
#' @export
#'
#' @examples
#' simRaschdat(10,ilocation=NULL,plocation=rnorm(500,0,1),rnum=0.6)
#' \dontrun{
#' simRaschdat(20,nobs=50,rnum=0.6)
#' }
simRaschdat <- function(nitem,ilocation=NULL,plocation=NULL,nobs=0,rnum=0.5){

  start.time <- Sys.time()


  if(is.null(ilocation)){
    ilocation = rnorm(nitem,0,1)
  }

  if(nobs==0 | !is.null(plocation)) {
    plocation
  } else {
    plocation = rnorm(nobs,0,1)
  }

  if(class(ilocation)=="data.frame"|class(plocation)=="data.frame"){
    stop("To implement the data generation, the parameter data must be specified in the argument.",
         call. = FALSE)
  }

  i.loc = na.omit(ilocation)
  p.loc = na.omit(plocation)

  p.matrix = matrix(rep(p.loc, nitem), ncol = nitem)

  expo = t(apply( p.matrix, 1, '-', i.loc))

  p = exp(expo)/(1 + exp(expo))

  resp.prob = matrix(p, ncol = nitem)

  #obs.resp = matrix(sapply(c(resp.prob), rbinom, n = 1, size = 1), ncol = nitems)

  if(rnum==0.5){
    obs.resp = matrix(ifelse(resp.prob >= 0.5,1,0), ncol = nitem)
  }else{
    obs.resp = matrix(ifelse(resp.prob >= rnum,1,0), ncol = nitem)
  }

  output = list()
  output$iloc = i.loc
  output$ploc = p.loc
  output$resp = obs.resp

  end.time = Sys.time()
  time_interval <- round(as.numeric(difftime(end.time, start.time,units = "secs")), 2)
  output

}
