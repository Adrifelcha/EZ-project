# Sanity checks
sim3 <- simStudy3$sim_indiv
nParticipants <- nP3
nTrials <- nT

x.drift <- sim3[,,"drift"]
x.bound <- sim3[,,"bound"]
x.ndt <- sim3[,,"nondt"]
problem.drift <- which(round(x.drift[,"est"],0)==0&round(x.drift[,"true"],0)==-1)


plot(x.drift[problem.drift,1],x.drift[problem.drift,2], xlim=c(-1,0.5), ylim=c(-1,0.5), xlab="true")
abline(0,1)
plot(x.bound[problem.drift,1],x.bound[problem.drift,2], xlim=c(-0.4,2.5), ylim=c(-0.4,2.5), xlab="true")
abline(0,1)
plot(x.ndt[problem.drift,1],x.ndt[problem.drift,2], xlim=c(-0.1,0.6), ylim=c(-0.1,0.6), xlab="true")
abline(0,1)


k <- ceiling(problem.drift[2]/nParticipants)
set.seed(k)
n.chains = 4
Show=TRUE

design.parameters <- Hddm_Parameter_Set(nParticipants,nTrials, Show=Show)
settings <- design.parameters$settings
parameter_set <- design.parameters$parameter_set
getData <- Hddm_Data(settings,parameter_set)

modelFile="./EZHBDDM.bug"
write_JAGSmodel(settings$prior)
# Load settings
parameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                "drift_sdev", "nondt_sdev", "bound_sdev", "drift")
myinits    <- default_inits(n.chains, settings$nPart)
data <- getData$jagsData
# Prepare data
sub     <- getData$sumData$sub
correct <- getData$sumData$sum_correct
varRT   <- getData$sumData$varRT_correct
meanRT  <- getData$sumData$meanRT_correct
nTrialsPerPerson <- as.numeric(unique(tapply(getData$rawData$accuracy,getData$rawData$sub,length)))
nParticipants    <- length(getData$sumData$sub)
# Run model and get samples
suppressMessages(library(R2jags))
suppressMessages(samples <- jags(data=data, 
                                 parameters.to.save=parameters, 
                                 model=modelFile, 
                                 n.chains=n.chains, 
                                 n.iter=4000, 
                                 n.burnin=200, 
                                 n.thin=1, 
                                 DIC=T, 
                                 inits=myinits))
library(rstan)
object <- samples$BUGSoutput$sims.array
Rhats <- apply(object,3,Rhat)
hist(Rhats)
abline(v=1.01)

drift.all <- extractSamples("drift", samples)
drift_mean.all = extractSamples("drift_mean", samples)
drift_sdev.all = extractSamples("drift_sdev", samples)
bound.all = extractSamples("bound", samples)
bound_mean.all = extractSamples("bound_mean", samples)
bound_sdev.all = extractSamples("bound_sdev", samples)
nondt.all = extractSamples("nondt", samples)
nondt_mean.all = extractSamples("nondt_mean", samples)
nondt_sdev.all = extractSamples("nondt_sdev", samples)





drift = apply(drift.all,3,mean) 
bound = apply(bound.all,3,mean) 
nondt = apply(nondt.all,3,mean) 
drift_mean = mean(drift_mean.all)
drift_sdev = mean(drift_sdev.all)  
bound_mean = mean(bound_mean.all)
bound_sdev = mean(bound_sdev.all)
nondt_mean = mean(nondt_mean.all)
nondt_sdev = mean(nondt_sdev.all)

estimates <- list("drift" = drift, "bound" = bound, "nondt" = nondt, "drift_mean" = drift_mean,
                  "drift_sdev" = drift_sdev, "bound_mean" = bound_mean, "bound_sdev" = bound_sdev,
                  "nondt_mean" = nondt_mean, "nondt_sdev" = nondt_sdev)

error <- getError(estimates, parameter_set)


plot(x.drift[problem.drift,1],x.drift[problem.drift,2], xlim=c(-1,0.5), ylim=c(-1,0.5), xlab="true", cex=1.2, pch=16, col="gray80")
points(x.drift[problem.drift[problem.drift < nParticipants],1],x.drift[problem.drift[problem.drift < nParticipants],2])
points(parameter_set$drift[problem.drift[problem.drift < nParticipants]], estimates$drift[problem.drift[problem.drift < nParticipants]], col="red", pch=3)
abline(0,1)

plot(x.bound[problem.drift,1],x.bound[problem.drift,2], xlim=c(-0.4,2.5), ylim=c(-0.4,2.5), xlab="true", cex=1.2, pch=16, col="gray80")
points(x.bound[problem.drift[problem.drift < nParticipants],1],x.bound[problem.drift[problem.drift < nParticipants],2])
points(parameter_set$bound[problem.drift[problem.drift < nParticipants]], estimates$bound[problem.drift[problem.drift < nParticipants]], col="red", pch=3)
abline(0,1)

plot(x.ndt[problem.drift,1],x.ndt[problem.drift,2], xlim=c(-0.1,0.6), ylim=c(-0.1,0.6), xlab="true", cex=1.2, pch=16, col="gray80")
points(x.ndt[problem.drift[problem.drift < nParticipants],1],x.ndt[problem.drift[problem.drift < nParticipants],2])
points(parameter_set$nondt[problem.drift[problem.drift < nParticipants]], estimates$nondt[problem.drift[problem.drift < nParticipants]], col="red", pch=3)
abline(0,1)