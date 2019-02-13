#' inverse.logit
#'
#' Backtransforms a logit transformation that incolved rescaling as per car.
#'
#' @param  x A vector of values for which to apply a logit back-transformation
#'
#' @param  car.rescale Ture/FALSE indicator for if car based rescaling is required.
#'
#' @export
#' @return A vector of logit-backtransformed values.
#'
inverse.logit <- function(x,car.rescale) {
   x[which(x>709.7827128933840299)]=709.7827128933840299
   if(car.rescale==T){
      a=0.025
      a <- (1-2*a)
      return(((a*(1+exp(x))+(exp(x)-1))/(2*a*(1+exp(x)))))}else{
      return(exp(x) / (1 + exp(x)))}
}









