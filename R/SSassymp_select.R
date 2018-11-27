
#' fit.SSasymp
#'
#' Fits a range of assymptotic models and returns a list of model fits.
#'
#' @param  data The data frame containing the x and y to which the model should be fit.
#' Note these must currently by labelled x and y
#'
#' @param  exp.val The exponent to use for generate the search grid of potential
#' starting values. Large values will make the function very slow to run.
#'
#' @export
#' @return A list of fitted assymptotic models that can be sued for multimodel inference or
#' to select the best model (ie based on the lowest AICc).
#'

fit.SSasymp=function(data,exp.val=2){
  require(minpack.lm)
  require(plyr)

  x=data$x
  y=data$y
  x.range=range(x,na.rm=T)
  x.sequence=seq(x.range[1],x.range[2],length=100)
  seq.vec=c(-10^(-exp.val:exp.val),0,10^(-exp.val:exp.val))
  start.dat=expand.grid(a=seq.vec,b=seq.vec,c=seq.vec,d=seq.vec)

  # MM  y ~ (a*x)/(1+(b*x))
  start.dat.mm=unique(start.dat[,c("a","b")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                                      (start.dat.mm$a*x)/(1+(start.dat.mm$b*x))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.mm=as.list(start.dat.mm[which.min(tt),])
  mm.fit=try(nlsLM(y ~ (a*x)/(1+(b*x)),data = data,start=start.vals.mm,
                   control=list(maxiter=1000)),silent=T)

  #NE y~a*(1-exp((-b)*x))
  start.dat.ne=unique(start.dat[,c("a","b")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                                    start.dat.ne$a*(1-exp((-start.dat.ne$b)*x))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.ne=as.list(start.dat.ne[which.min(tt),])
  ne.fit=try(nlsLM(y~a*(1-exp((-b)*x)),data = data,start=start.vals.ne,
                   control=list(maxiter=1000)),silent=T)

  #Asy y~a-(b*(c^x))
  start.dat.asy=unique(start.dat[,c("a","b","c")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                          start.dat.asy$a-(start.dat.asy$b*(start.dat.asy$c^x))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.asy=as.list(start.dat.asy[which.min(tt),])
  asy.fit=try(nlsLM(y~a-(b*(c^x)),data = data,start=start.vals.asy,
                   control=list(maxiter=1000)),silent=T)

  #CR y~a*((1-exp(-b*x))^c)
  start.dat.cr=unique(start.dat[,c("a","b","c")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                     start.dat.cr$a*((1-exp(-start.dat.cr$b*x))^start.dat.cr$c)}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.cr=as.list(start.dat.cr[which.min(tt),])
  cr.fit=try(nlsLM(y~a*((1-exp(-b*x))^c),data = data,start=start.vals.cr,
                   control=list(maxiter=1000)),silent=T)

  #Rat y~(a+(b*x))/(1+(c*x))
  start.dat.rat=unique(start.dat[,c("a","b","c")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                  (start.dat.rat$a+(start.dat.rat$b*x))/(1+(start.dat.rat$c*x))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.rat=as.list(start.dat.rat[which.min(tt),])
  rat.fit=try(nlsLM(y~(a+(b*x))/(1+(c*x)),data = data,start=start.vals.rat,
                   control=list(maxiter=1000)),silent=T)

  #Weib y~a*(1-(exp(-((b*(x-c))^d))))
  start.dat.weib=unique(start.dat[,c("a","b","c","d")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
           start.dat.weib$a*(1-(exp(-((start.dat.weib$b*(x-start.dat.weib$c))^start.dat.weib$d))))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.weib=as.list(start.dat.weib[which.min(tt),])
  weib.fit=try(nlsLM(y~a*(1-(exp(-((b*(x-c))^d)))),data = data,start=start.vals.weib,
                   control=list(maxiter=1000)),silent=T)

  #logit y~a*(b*exp(c*x)/(1+b*exp(c*x))
  start.dat.logit=unique(start.dat[,c("a","b","c")])
  y.pred=do.call(cbind,alply(x,.margins=1,.fun=function(x){
                start.dat.logit$a*(start.dat.logit$b*exp(start.dat.logit$c*x)/(1+start.dat.logit$b*exp(start.dat.logit$c*x)))}))
  tt=unlist(alply(y.pred,.margin=1,.fun=function(y.pred){sum((y-y.pred)^2)}))
  start.vals.logit=as.list(start.dat.logit[which.min(tt),])
  logit.fit=try(nlsLM(y~a*(b*exp(c*x)/(1+b*exp(c*x))),data = data,start=start.vals.logit,
                   control=list(maxiter=1000)),silent=T)

  # collate the fitted models into a list
  out.all.mods=list(mm.fit,ne.fit,asy.fit,cr.fit,rat.fit,weib.fit,logit.fit)
  names(out.all.mods)=c("mm","ne","asy","cr","rat","weib","logit")

  # plot them (if desired)
  #plot(x,y,pch=1)
  #for(f in 1:length(out.all.mods)){
  #  if(class(out.all.mods[[f]])!="try-error"){
  #  lines(x.sequence,
  #      predict(out.all.mods[[f]],newdata=data.frame(x=x.sequence)),col=f)
  #}}
#  legend("bottomright",legend=names(out.all.mods),col=1:length(out.all.mods),lty=1)
  return(out.all.mods)
}

# fit=fit.SSasymp(data=data.frame(y=bruv$sig.outcomes[plot.i.index],
#                                  x=bruv$effect[plot.i.index]),exp.val=1.5)
#
#  aic.fits=laply(fit,.fun=function(x){
#    if(class(x)!="try-error"){AIC(x)}else{NA}})
#  best.mod=names(fit)[which.min(aic.fits)]
#  out.probs=predict(fit[[best.mod]])



