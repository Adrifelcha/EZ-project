par(mfrow=c(3,2))

# Choosing priors for drift_mean
x <- seq(-5,5,0.01)
mean <- 0
sd <- 2.5
plot(x,dnorm(x,mean,sd),type="l", main=paste("Drift mean: mean =", mean, ", sd =", sd))
qnorm(c(0.01,0.99),mean,sd)
# JAGS uses gamma shape and rate
# Choosing priors for drift_sdev
shape = 2
rate = 0.2
x <- 1/seq(0,1.2,0.01)
plot(1/x,dgamma(x,shape,rate),type="l",main=paste("Drift sdev: shape =", shape, ", rate =", rate))

# Choosing priors for bound_mean
x <- seq(0.5,4,0.01)
mean <- 2.25
sd <- 0.8
plot(x,dnorm(x,mean,sd),type="l", main=paste("Bound mean: mean =", mean, ", sd =", sd))
qnorm(c(0.01,0.99),mean,sd)
# Choosing priors for bound_sdev
shape = 3
rate = 0.5
x <- 1/seq(0,0.8,0.01)
plot(1/x,dgamma(x,shape,rate),type="l",main=paste("Bound sdev: shape =", shape, ", rate =", rate))

# Choosing priors for nondt_mean
x <- seq(0.2,1,0.01)
mean <- 0.55
sd <- 0.15
plot(x,dnorm(x,mean,sd),type="l", main=paste("Nondt mean: mean =", mean, ", sd =", sd))
qnorm(c(0.01,0.99),mean,sd)
# Choosing priors for nondt_sdev
shape = 4
rate = 0.25
x <- 1/seq(0,0.2,0.01)
plot(1/x,dgamma(x,shape,rate),type="l",main=paste("Nondt sdev: shape =", shape, ", rate =", rate))


source("../../../../code/functions/default_priors.R")
source("../../../../code/functions/data_toJAGS.R")
source("../../../../code/functions/sample_parameters.R")
source("../../../../code/functions/write_JAGSmodel.R")
source("../../../../code/functions/default_inits.R")
source("../../../../code/scripts/HDDM_runJAGS.R")
source("../../../../code/scripts/HDDM_runSims.R")
x <- HDDM_runSims(nParticipants = 20,nTrials = 20,nDatasets = 50,priors = NA,
                  modelType = "metaregression",criterion = "bound", n.chains = 4,
                  Show = TRUE,forceSim = TRUE, fromPrior = FALSE)
plot_recovery(x)
check_Rhat(x$rhats)