

#fit.SSasymp.Bayes=function(data,SSasymp.fit){

### mm (a * x)/(1 + (b * x))
fun.mm <- function(x){
     mean((mcmc.i$a * x)/(1 + (mcmc.i$b * x)))
  }
sink("JAGS_mm.txt")
cat("
  model{
  #Likelihood
  for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- (a * X[i])/(1 + (b * X[i])) + r[re.ID[i]]
  }

 #--Priors betas
  a  ~ dnorm(0, 0.1)
  b  ~ dnorm(0, 0.1)

  #--Priors random effects
  for (i in 1:Num.ID) { r[i] ~ dnorm(0, tau_ID)}

 #--Priors for sigmas
  #random variance
  tau_ID <- 1 / sigma_ID^2
  num_ID           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom_ID         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma_ID   <- abs(num_ID / denom_ID)           #<----half-Cauchy(25)

  # error variance
  tau <- 1 / sigma^2
  num           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma   <- abs(num / denom)           #<----half-Cauchy(25)
  }
  ",fill = TRUE)
sink()

### ne

### asy  a - (b * (c^x))
fun.asy <- function(x){
     mean(mcmc.i$a - (mcmc.i$b * (mcmc.i$c^x)))
  }
sink("JAGS_asy.txt")
cat("
  model{
  #Likelihood
  for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- a - (b * (c^X[i])) + r[re.ID[i]]
  }

 #--Priors betas
  a  ~ dnorm(0, 0.1)
  b  ~ dnorm(0, 0.1)
  c  ~ dnorm(0, 0.1)

  #--Priors random effects
  for (i in 1:Num.ID) { r[i] ~ dnorm(0, tau_ID)}

 #--Priors for sigmas
  #random variance
  tau_ID <- 1 / sigma_ID^2
  num_ID           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom_ID         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma_ID   <- abs(num_ID / denom_ID)           #<----half-Cauchy(25)

  # error variance
  tau <- 1 / sigma^2
  num           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma   <- abs(num / denom)           #<----half-Cauchy(25)
  }
  ",fill = TRUE)
sink()



### cr


### rat


### weib (a * (1 - ( exp (-((b * (x - c))^d)))))
fun.weib <- function(x){
     mean(mcmc.i$a * (1 - ( exp (-((mcmc.i$b * (x - mcmc.i$c))^mcmc.i$d)))))
  }
sink("JAGS_weib.txt")
cat("
  model{
  #Likelihood
  for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- (a * (1 - ( exp (-((b * (X[i] - c))^d)))))  + r[re.ID[i]]
      }

 #--Priors betas
  a  ~ dnorm(0, 0.1)
  b  ~ dnorm(0, 0.1)
  c  ~ dnorm(0, 0.1)
  d  ~ dnorm(0, 0.1)


  #--Priors random effects
  for (i in 1:Num.ID) { r[i] ~ dnorm(0, tau_ID)}

 #--Priors for sigmas
  #random variance
  tau_ID <- 1 / sigma_ID^2
  num_ID           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom_ID         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma_ID   <- abs(num_ID / denom_ID)           #<----half-Cauchy(25)

  # error variance
  tau <- 1 / sigma^2
  num           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma   <- abs(num / denom)           #<----half-Cauchy(25)
  }
  ",fill = TRUE)
sink()


### logit  a * (b * exp(c * x)/(1 + b * exp(c * x)))
fun.logit <- function(x){
     mean( mcmc.i$a * (mcmc.i$b * exp(mcmc.i$c * x)/(1 + mcmc.i$b * exp(mcmc.i$c * x))) )
  }
sink("JAGS_logit.txt")
cat("
  model{
  #Likelihood
  for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <-  a * (b * exp(c * X[i])/(1 + b * exp(c * X[i]))) + r[re.ID[i]]
  }

 #--Priors betas
  a  ~ dnorm(0, 0.1)
  b  ~ dnorm(0, 0.1)
  c  ~ dnorm(0, 0.1)

  #--Priors random effects
  for (i in 1:Num.ID) { r[i] ~ dnorm(0, tau_ID)}

 #--Priors for sigmas
  #random variance
  tau_ID <- 1 / sigma_ID^2
  num_ID           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom_ID         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma_ID   <- abs(num_ID / denom_ID)           #<----half-Cauchy(25)

  # error variance
  tau <- 1 / sigma^2
  num           ~ dnorm(0, 0.0016)                #<----half-Cauchy(25)
  denom         ~ dnorm(0, 1)                     #<----half-Cauchy(25)
  sigma   <- abs(num / denom)           #<----half-Cauchy(25)
  }
  ",fill = TRUE)
sink()













