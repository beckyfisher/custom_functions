#' wi
#'
#' Calculates model weights based on AIC, AICc, or BIC
#'
#' @param AIC.vals A vector of AIC, AICc or BIC values
#'
#' @return A vector of Akaike weights
#'
wi<<-function(AIC.vals){# This function calculate the Aikaike weights:
 # wi=(exp(-1/2*AICc.vals.adj))/Sum.wi=1 to r (exp(-1/2*AICc.vals.adj))
  # Where r is the number of models examined.

 #AIC.vals=model.data.out$BIC

 AICc.vals.adj=AIC.vals-min(na.omit(AIC.vals))
 wi.den=rep(NA,length(AICc.vals.adj))
 for(i in 1:length(AICc.vals.adj)){
  wi.den[i]=exp(-1/2*AICc.vals.adj[i])}
 wi.den.sum=sum(na.omit(wi.den))
 wi=wi.den/wi.den.sum
return(wi)}