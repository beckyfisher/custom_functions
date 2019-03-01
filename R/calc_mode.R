#' calc.mode
#'
#' Calculates a model value, using the most appropriate method given the data type.
#'
#' @param  x A vector of values for which to calculate a mode
#'
#' @param  type The type of mode to calculate for numeric vectors
#'
#' @export
#' @return A single value that is the estimated mode.
#'
#' @details By default this uses table(x) for characters and factor vectors,
#' type=1 is for numeric values using a table based method (same definition as for character and factor variables),
#' type=2 is based on kernal density, and type=3 is based on a more complex kernal density estimation method
#' by Ted Harding & Douglas Bates https://stat.ethz.ch/pipermail/r-help/2008-August/172323.html
calc.mode <-function(x,type=1) {
  # mode is simply defined for characters or factors

  if(length(na.omit(x))>1){
    x=na.omit(x)
    if(is.factor(x)==T | is.character(x)==T){
  	 mode.estimate=names(which.max(table(x)))
    }
    # for numeric values, simple method (same definition as above)
    if(is.numeric(x)==T & type==1) {
  	 mode.estimate=as.numeric(names(which.max(table(x))))
    }
    # based on kernal desnity
    if(is.numeric(x)==T & type==2) {
     dd <- density(na.omit(x))
     mode.estimate=as.numeric(dd$x[which.max(dd$y)])
    }
    # based on more complex method
    if(is.numeric(x)==T & type==3) {
     # Function for mode estimation of a continuous variable
     # Kernel density estimation by Ted Harding & Douglas Bates (found on
     #RSiteSearch)
     #https://stat.ethz.ch/pipermail/r-help/2008-August/172323.html

      x=na.omit(x)
      lim.inf=min(x)-1; lim.sup=max(x)+1

      hist(x,#freq=FALSE,
      breaks=seq(lim.inf,lim.sup,0.2),plot=F)
      s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
      n<-length(s$y)
      v1<-s$y[1:(n-2)];
      v2<-s$y[2:(n-1)];
      v3<-s$y[3:n]
      ix<-1+which((v1<v2)&(v2>v3))
      mode.estimate=s$x[which(s$y==max(s$y))]
     }
  }
  if(length(na.omit(x))==1){mode.estimate=na.omit(x)}
  if(length(na.omit(x))==0){mode.estimate=NA}
return(mode.estimate)
}

