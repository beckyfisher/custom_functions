run.iters=function(new.dat.all.iter,
                   niter=100,
                   n.colony.reps=100,
                   new.dat.base,mod.list,
                   group.g,
                   ncores=4){
 inverse.logit=function(x,car.rescale) {
   x[which(x>709.7827128933840299)]=709.7827128933840299
   if(car.rescale==T){
      a=0.025
      a <- (1-2*a)
      return(((a*(1+exp(x))+(exp(x)-1))/(2*a*(1+exp(x)))))}else{
      return(exp(x) / (1 + exp(x)))}}
 stan.model.bleaching=mod.list$bleaching
 models.mort=mod.list$mort
 models.prop=mod.list$prop
 sm.fit.stan=mod.list$sm

 require(doParallel)
 cl=makePSOCKcluster(4)
 registerDoParallel(cl)
 NEWITERS<-foreach(l = 1:niter,
                   .packages=c('rstanarm','car','doBy'),
                   .errorhandling='pass')%dopar%{
  new.dat=new.dat.base
  new.dat$logit.light=logit(new.dat$light)
  # get a posteropr sample for sedimentation based on light
  new.dat$sedimentation=inverse.logit(t(posterior_predict(sm.fit.stan,
                                          newdata=new.dat,
                                          re.form=NA,
                                          draws=1)),car.rescale=F)
  new.dat$bleach.event=0
  new.dat$b.trials=n.colony.reps#n.colonies[g,"index.sum"] # assume same number of colonies as observed
  new.dat$mort.event=0
  new.dat$trials=100  # assume start of 100% cover units per colony
  new.dat$mortality=0
  new.dat$use.group=group.g

  # probability of bleaching
  new.dat$bleach.event=posterior_predict(stan.model.bleaching,
                                    new.dat,draws=1,re.form=NA)[1,]
  #new.dat$bleach.event/new.dat$b.trials
  new.dat.1=new.dat[rep(1:nrow(new.dat), new.dat$bleach.event),]
  new.dat.1$bleaching=1
  new.dat.0=new.dat[rep(1:nrow(new.dat), new.dat$b.trials-new.dat$bleach.event),]
  new.dat.0$bleaching=0
  new.dat=rbind(new.dat.1,new.dat.0)
  rownames(new.dat)=1:nrow(new.dat)
  new.dat$bleaching=as.factor(new.dat$bleaching)

  new.dat$mortality=posterior_predict(models.mort[[group.g]],
                                         new.dat,draws=1,re.form=NA)[1,]
  new.dat$loss=(posterior_predict(models.prop[[group.g]],
                                    new.dat,draws=1,re.form=NA)/100)[1,]
  # total mortality is the product of mortality (given bleaching) and loss
  new.dat$total=new.dat$mortality*new.dat$loss
  new.dat=summaryBy(total~temperature+light,data=new.dat,FUN=mean,keep.names=T)
 }
 stopCluster(cl)
 registerDoSEQ()
 new.dat.all.iter=c(new.dat.all.iter,NEWITERS)
return(new.dat.all.iter)}

#### function for observed data (has sedimentation and no need to esimtate from light)
run.iters.observed=function(new.dat.all.iter,
                   niter=100,
                   n.colony.reps=100,
                   new.dat.base,
                   mod.list,
                   group.g,
                   ncores=4){
 inverse.logit=function(x,car.rescale) {
   x[which(x>709.7827128933840299)]=709.7827128933840299
   if(car.rescale==T){
      a=0.025
      a <- (1-2*a)
      return(((a*(1+exp(x))+(exp(x)-1))/(2*a*(1+exp(x)))))}else{
      return(exp(x) / (1 + exp(x)))}}
 stan.model.bleaching=mod.list$bleaching
 models.mort=mod.list$mort
 models.prop=mod.list$prop
 sm.fit.stan=mod.list$sm

 require(doParallel)
 cl=makePSOCKcluster(4)
 registerDoParallel(cl)
 NEWITERS<-foreach(l = 1:niter,
                   .packages=c('rstanarm','car','doBy'),
                   .errorhandling='pass')%dopar%{
  new.dat=new.dat.base
  new.dat$bleach.event=0
  new.dat$b.trials=n.colony.reps#n.colonies[g,"index.sum"] # assume same number of colonies as observed
  new.dat$mort.event=0
  new.dat$trials=100  # assume start of 100% cover units per colony
  new.dat$use.group=group.g

  # probability of bleaching
  new.dat$bleach.event=posterior_predict(stan.model.bleaching,
                                    new.dat,draws=1,re.form=NA)[1,]
  #new.dat$bleach.event/new.dat$b.trials
  new.dat.1=new.dat[rep(1:nrow(new.dat), new.dat$bleach.event),]
  new.dat.1$bleaching=1
  new.dat.0=new.dat[rep(1:nrow(new.dat), new.dat$b.trials-new.dat$bleach.event),]
  new.dat.0$bleaching=0
  new.dat=rbind(new.dat.1,new.dat.0)
  rownames(new.dat)=1:nrow(new.dat)
  new.dat$bleaching=as.factor(new.dat$bleaching)

  new.dat$mortality=posterior_predict(models.mort[[group.g]],
                                         new.dat,draws=1,re.form=NA)[1,]
  new.dat$loss=(posterior_predict(models.prop[[group.g]],
                                    new.dat,draws=1,re.form=NA)/100)[1,]
  # total mortality is the product of mortality (given bleaching) and loss
  new.dat$total=new.dat$mortality*new.dat$loss
  new.dat=summaryBy(total~Site.Code+fn+distance+SSC+light+sedimentation+temperature,
     data=new.dat,FUN=mean,keep.names=T)
 }
 stopCluster(cl)
 registerDoSEQ()
 new.dat.all.iter=c(new.dat.all.iter,NEWITERS)
return(new.dat.all.iter)}

