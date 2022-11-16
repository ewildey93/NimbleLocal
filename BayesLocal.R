library(nimble)
library(MCMCvis)
CalibDS <- read.csv("//josh-nas/NetBackup/Eli-Backup/Documents/R/data_tools/data_tools/Calibration1.20/Calibration_Dataset_withResiduals_Predictions.csv")
calibration <- read.csv("//josh-nas/NetBackup/Eli-BackUp/Documents/R/data_tools/data_tools/Calibration1.20/Calibration1.20.csv")
colnames(calibration)[2] <- "TestId"
CalibDS2 <- merge(CalibDS, calibration[,c(2,5)], by="TestId")
CalibDS2 <- CalibDS2[order(CalibDS2$NodeId, CalibDS2$TagId),]
CalibDS2$fNodeId<- as.factor(CalibDS2$NodeId)
CalibDS2$fTagId <- as.factor(CalibDS2$TagId)
CalibDS3 <- CalibDS2
CalibDS3$NodeId <- as.numeric(CalibDS3$fNodeId)
CalibDS3$TagId <- as.numeric(CalibDS3$fTagId)
sample <- CalibDS[1:2,]

#######model without known distance/calibration data##########
model <- nimbleCode({
  #process model
  for (i in 1:n) {
    avgRSS[i] ~ dnorm(a * exp(-S * distance[i]) + K, vari)
  }
  #priors
  a ~ dnorm(32.57, 10)
  
  S ~ dgamma(shape = 0.1759, rate = 17.6828)# this might not be a beta dbeta(.0485, 39.105), dnorm(.009949, .02372)
  
  K ~ dnorm(-103.86, 8.567)
  
  for (i in 1:n) {
  distance[i] ~ dgamma (2.41, .005) #see scrap
  }
  vari ~ dinvgamma(1.72, 1.87957)#see scrap
  
  #RSSI? Its known but predicting locations from data we used to build model seems fishy
  
  #derived quantity-location?
})


my.data <- list(avgRSS=CalibDS$avgRSS)
RSSconst <- list(n=length(CalibDS$avgRSS))

parameters.to.save <- c('a', 'S', 'K', "distance")

init1 <- list(a=rnorm(1,32.57, 69.421), S=rgamma(1,0.1759, rate=17.6828), K=rnorm(1,mean=-103.86, sd=8.567), vari=rinvgamma(1,1.72, 1.87957), distance=rgamma(10214,2.41, .005))

n.iter <- 10000
n.burnin <- 3000
n.chains <- 3

mcmc.output <- nimbleMCMC(code = model,
                          data = my.data,
                          inits = init1,
                          monitors = parameters.to.save,
                          constants=RSSconst,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)


MCMCsummary(object = mcmc.output, round=4, params = c("a","S","K"))


MCMCtrace(object = mcmc.output,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = c("a","S","K"))

head(mcmc.output$chain1)
mean(mcmc.output$chain1[,'distance[1]'])
x <- MCMCsummary(object =mcmc.output , round=4,params = "distance")
y <- x[,1:2]

c <- cbind(CalibDS[,6], y[,1:2])
str(c)
ggplot(c, aes(CalibDS[,6], mean)) + 
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Actual Distance", y = "Distance Est.") + 
  #geom_text(aes(label=Test.Group),check_overlap = TRUE) +
  theme_classic()

#######model with known distance/calibration####################
model2 <- nimbleCode({
  #process model
   for  (i in 1:n){
     avgRSS[i] ~ dnorm(a * exp(-S * distance[i]) + K, sd=vari)
   }
  #priors
  a ~ dnorm(32.57, 69.421)
  
  S ~ dnorm(.009949, .02372)# this might not be a normal, maybe beta
  
  K ~ dnorm(-103.86, 8.567)
  
  vari ~ dinvgamma(shape=1.72, rate=1.87957)#see scrap
  
  #RSSI? Its known but predicting locations from data we used to build model seems fishy
  
  #derived quantity-location?
})
model

#localmodel <- nimbleModel(code=model, name="localmodel", constants=RSSconst, data=my.data, inits = init2)

RSSconst2 <- list(n=length(CalibDS$avgRSS))
my.data2 <- list(avgRSS=CalibDS$avgRSS, distance=CalibDS$distance)

parameters.to.save2 <- c('a', 'S', 'K')

#To make sure that the MCMC algorithm explores the posterior distribution, 
#we start different chains with different parameter values
#theta=survival so starting values of 10%, 50%, and 90%
init1 <- list(a=rnorm(3,32.57, 69.421), S=rnorm(3,.009949, .02372), K=rnorm(3,mean=-103.86, sd=8.567), vari=rinvgamma(3,1.72, 1.87957))
init2 <- list(a=rnorm(1,32.57, 69.421), S=rnorm(1,.009949, .02372), K=rnorm(1,-103.86, 8.567), vari=rinvgamma(1,1.72, 1.87957))
init3 <- list(a=rnorm(1,32.57, 69.421), S=rnorm(1,.009949, .02372), K=rnorm(1,-103.86, 8.567), vari=rinvgamma(1,1.72, 1.87957))
initial.values <- list(init1, init2, init3)
# OR set starting value to random number
initial.values <- function() list(init1 = rnorm(32.57, 69.421), )

#set up number of chains, iterations and burn in
n.iter <- 10000
n.burnin <- 3000
n.chains <- 3

#run the model!
#argument summary=TRUE gives summary
mcmc.output2 <- nimbleMCMC(code = model2,
                          data = my.data2,
                          inits = init2,
                          monitors = parameters.to.save2,
                          constants=RSSconst2,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)

#Visualization
MCMCsummary(object = mcmc.output2, round=4)


MCMCtrace(object = mcmc.output2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "all")
#####################sample####################################
model <- nimbleCode({
  #process model
  for  (i in 1:n){
    avgRSS[i] ~ dnorm(a * exp(-S * distance[i]) + K, sd=vari)
  }
  #priors
  a ~ dnorm(32.57, 69.421)
  
  S ~ dnorm(.009949, .02372)# this might not be a normal, maybe beta
  
  K ~ dnorm(-103.86, 8.567)
  
  vari ~ dinvgamma(shape=1.72, rate=1.87957)#see scrap
  
  #RSSI? Its known but predicting locations from data we used to build model seems fishy
  
  #derived quantity-location?
})
model

Sampleconst <- list(n=length(sample$avgRSS))
sample.data <- list(avgRSS=sample$avgRSS, distance=sample$distance)

parameters.to.save <- c('a', 'S', 'K')

localmodel <- nimbleModel(code=model, name="localmodel", constants=Sampleconst, data=sample.data, inits = init2)
localmodel$plotGraph()

####################################################################################################################
###############more complicated model-unknown distance######################################
####################################################################################################################
#model incorporating effects of nodes on a,s and K, and effect of tags on a
model3 <- nimbleCode({
  #process model
  for (i in 1:n) {
    avgRSS[i] ~ dnorm((RE[TagId[i]] + a[NodeId[i]]) * exp(-S[NodeId[i]] * distance[i]) + K[NodeId[i]], vari)
  }
  #priors for effect of nodes on a,s,and K
 for (i in 1:nodes) {
    a[i] ~ dnorm(32.57, 69.421)
  
  K[i] ~ dnorm(-103.86, 8.567)
  
  S[i] ~ dnorm(.009949, .02372)# this might not be a normal, maybe beta
 }
  for (i in 1:n) {
    distance[i] ~ dgamma (2.41, .005) #see scrap, should this be lognormal?
  }
  #prior for random effect of tag on intercept, not sure this should be incorporate in this way
  for (i in 1:tags) {
    RE[i] ~ dnorm(0, 100)
  }
  vari ~ dinvgamma(1.72, 1.87957)#see scrap
  
  distance ~ dlognorm(equation I need to figure out here)#is this in the right place
  #RSSI? Its known but predicting locations from data we used to build model seems fishy
  

})
model3

my.data <- list(avgRSS=CalibDS3$avgRSS)
RSSconsts <- list(n=length(CalibDS3$avgRSS), nodes=length(unique(CalibDS3$NodeId)), tags=length(unique(CalibDS3$TagId)), NodeId=CalibDS3$NodeId, TagId=CalibDS3$TagId)

init1 <- list(a=rnorm(76,32.57, 69.421), S=rnorm(76,.009949, .02372), K=rnorm(76,mean=-103.86, sd=8.567), vari=rinvgamma(1,1.72, 1.87957), distance=rgamma(10214,2.41, .005), RE=rnorm(26,0,100))

parameters.to.save <- c("distance", "a", "S", "K", "RE")

Bayes <- nimbleModel(code = model3, name = "Bayes", constants = RSSconsts,
                    data = my.data, inits = init1)
#set up number of chains, iterations and burn in
n.iter <- 10000
n.burnin <- 3000
n.chains <- 3

mcmc.output3 <- nimbleMCMC(code = model3,
                           data = my.data,
                           inits = init1,
                           monitors = parameters.to.save,
                           constants=RSSconsts,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)

head(mcmc.output3$chain1)
x <- MCMCsummary(object = mcmc.output3, round=4,params = "distance")
y <- cbind(CalibDS3, x$mean)
K <- MCMCsummary(object = mcmc.output3, round=4,params = "K")
a <- MCMCsummary(object = mcmc.output3, round=4,params = "a")
S <- MCMCsummary(object = mcmc.output3, round=4,params = "S")

ggplot(y, aes(distance, x$mean)) + 
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Actual Distance", y = "Distance Est.") + 
  #geom_text(aes(label=Test.Group),check_overlap = TRUE) +
  theme_classic()

ggplot(y, aes(distance, log(x$mean))) + 
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Actual Distance", y = "Distance Est.") + 
  #geom_text(aes(label=Test.Group),check_overlap = TRUE) +
  theme_classic()

Sampleconst <- list(n=length(sample$avgRSS))
sample.data <- list(avgRSS=sample$avgRSS, distance=sample$distance)


###########################################################
#########more complicated model but with distance known####
###########################################################
#model incorporating effects of nodes on a,s and K, and effect of tags on a
model4 <- nimbleCode({
  #process model
  for (i in 1:n) {
    avgRSS[i] ~ dnorm((RE[TagId[i]] + a[NodeId[i]]) * exp(-S[NodeId[i]] * distance[i]) + K[NodeId[i]], vari)
  }
  #priors for effect of nodes on a,s,and K
  for (i in 1:nodes) {
    a[i] ~ dnorm(32.57, 69.421)
    
    K[i] ~ dnorm(-103.86, 8.567)
    
    S[i] ~ dnorm(.009949, .02372)# this might not be a normal, maybe beta
  }
  #prior for random effect of tag on intercept
  for (i in 1:tags) {
    RE[i] ~ dnorm(0, 100)
  }
  vari ~ dinvgamma(1.72, 1.87957)#see scrap
  
  #RSSI? Its known but predicting locations from data we used to build model seems fishy
  
  #derived quantity-location?
})
model4

my.data <- list(avgRSS=CalibDS3$avgRSS, distance=CalibDS3$distance)
RSSconsts <- list(n=length(CalibDS3$avgRSS), nodes=length(unique(CalibDS3$NodeId)), tags=length(unique(CalibDS3$TagId)), NodeId=CalibDS3$NodeId, TagId=CalibDS3$TagId)

init1 <- list(a=rnorm(76,32.57, 69.421), S=rnorm(76,.009949, .02372), K=rnorm(76,mean=-103.86, sd=8.567), vari=rinvgamma(1,1.72, 1.87957), RE=rnorm(26,0,100))

parameters.to.save <- c("a", "S", "K", "RE")

Bayes <- nimbleModel(code = model4, name = "Bayes", constants = RSSconsts,
                     data = my.data, inits = init1)
#set up number of chains, iterations and burn in
n.iter <- 10000
n.burnin <- 3000
n.chains <- 3

mcmc.output4 <- nimbleMCMC(code = model4,
                           data = my.data,
                           inits = init1,
                           monitors = parameters.to.save,
                           constants=RSSconsts,
                           niter = n.iter,
                           nburnin = n.burnin,
                           nchains = n.chains)


K2 <- MCMCsummary(object = mcmc.output4, round=4,params = "K")
a2 <- MCMCsummary(object = mcmc.output4, round=4,params = "a")
S2 <- MCMCsummary(object = mcmc.output4, round=4,params = "S")
RE <- MCMCsummary(object = mcmc.output4, round=4,params = "RE")

Ks <- cbind(K, K2)
as <- cbind(a, a2)
Ss <- cbind(S, S2)
############################################################################################
################scrap########################
hist(CalibDS$distance)
logdist <- log(CalibDS$distance)
hist(logdist)

library(MASS)
dist<-fitdistr(na.omit(CalibDS$distance[CalibDS$distance>0]), "gamma")
dist

hist(CalibDS$sdRSS)
dist2 <- fitdistr(1/(na.omit(CalibDS$sdRSS[CalibDS$sdRSS>0])), "gamma")
dist2


foo <- nimbleFunction( run = function(x = double(1)) {return(sum(x)); returnType(double())})
cfoo <- compileNimble(foo)
cfoo(1:10)
