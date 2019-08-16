#' xformEta
#'
#' Transforms and backtransforms between commonly link functions for a range of statistical distributions
#'
#' @param x A vector of values to transform
#'
#' @param variableType A character string indicating the type of distribution that has been used. Must be one of
#' 'gaussian', 'poisson', 'gamma', 'nbinomial', 'binomial', or 'beta'.
#'
#' @param direction A character string indication the direction to tranform x. Must be either 'response', or 'link'.
#' For 'response' x is assumed to be on the scale of the link function, and is backtransformed to the response scale.
#' For 'link' x is assumed to be on the scale of the response, and is transformed to the link scale.
#' Assumed link fuctions are as follows: 'gaussian' = identity, 'poisson' = log, 'gamma'  = log, 'nbinomial' - log, 'binomial' = logit, 'beta' = logit.
#'
#' @export
#' @return A vector of Akaike weights
#'
xformEta<-function(x,variableType,direction){
 if(direction=="response"){eta=x # from the scale of the link function to the response
  if(variableType=="gaussian"){P=eta}
  if(variableType=="poisson"){P=exp(eta)}
  if(variableType=="gamma"){P=exp(eta)}
  if(variableType=="nbinomial"){P=exp(eta)}
  if(variableType=="binomial"){P=exp(eta) / (1 + exp(eta))}
  if(variableType=="beta"){P=exp(eta) / (1 + exp(eta))}
  return(P)}
 if(direction=="link"){P=x  # from the response to the scale of the link function
  if(variableType=="gaussian"){eta=P}
  if(variableType=="poisson"){eta=log(P)}
  if(variableType=="gamma"){eta=log(P)}
  if(variableType=="nbinomial"){eta=log(P)}
  if(variableType=="binomial"){eta=qlogis(P)}
  if(variableType=="beta"){eta=qlogis(P)}
  return(eta)}
}
