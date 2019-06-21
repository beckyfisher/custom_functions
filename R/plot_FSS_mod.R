#' FSSplot
#'
#' Generates a plot on the raw scale of the response, for a gam model
#'
#' @param mod.r  The gam model objsect to plot
#'
#' @param y.lab An optional Y label for the response variable
#'
#' @export
#' @return
#'

FSSplot <- function(mod.r,y.lab="response"){
  dat.r <- mod.r[[1]]
  preds <- attributes(mod.r$terms)$term.labels
  family.val <- mod.r$family$family
  list.dat <- lapply(preds,FUN=function(x){
       x.dat <- dat.r[,x]
       seq(min(x.dat),max(x.dat),length=50)
      })
  names(list.dat) <- preds
  new.data.all <- expand.grid(list.dat)

  par(mfrow=c(1,length(preds)), mar=c(4,2,0.5,0.5), oma=c(0,2,0,0))

  for(p in 1:length(preds)){
    y <- mod.r$y
    x <- dat.r[,preds[p]]

    new.data.p <- summaryBy(
      as.formula(paste(paste(preds[-p],collapse="+"),"~",preds[p])),
      FUN=mean,
      keep.names=T,
      data=new.data.all)
    x.vec <- new.data.p[,preds[p]]
    pred.vals <- predict(mod.r,newdata = new.data.p, se=T)
    pred.y <- pred.vals$fit
    up <- pred.y + pred.vals$se*1.96
    lw <- pred.y - pred.vals$se*1.96

    if(family.val=="binomial"){
      pred.y <-inverse.logit(pred.vals$fit)
      up <-inverse.logit(pred.vals$fit + pred.vals$se*1.96)
      lw <-inverse.logit(pred.vals$fit - pred.vals$se*1.96)
    }


    # now make the plot
    y.lim <- range(c(up,lw))
    plot(x,y,pch=16, xlab=preds[p], ylim=y.lim,ylab="")
    lines(x.vec,pred.y)
    lines(x.vec,up, lty=2)
    lines(x.vec,lw,lty=2)
    if(p==1){mtext(side=2,outer=T,text=y.lab)}
   }

}
