# Load necessary libraries/packages
source(here("src", "plot_VerticalHist.R"))
source(here("src", "rhat.R"))
library(R2jags)
library(here)
seed <- 15

# Read the data file
data_raw <- read.csv(here("demos", "applications", "shape_perception", "data", "vpw08.csv"))

# Change column names
colnames(data_raw) <- c("sub", "change_quality", "change_type", "noChange", "response", "rt")

# Clean up data
# Remove RTs larger than 3 seconds
tmp <- data_raw[which(data_raw$rt<=3),]
# Identify each condition
change <- 1-tmp$noChange
cond <- rep(0,nrow(tmp))
cond[which(tmp$change_quality==0&tmp$change_type==0)] <- 1
cond[which(tmp$change_quality==1&tmp$change_type==0)] <- 2
cond[which(tmp$change_quality==0&tmp$change_type==1)] <- 3
cond[which(tmp$change_quality==1&tmp$change_type==1)] <- 4
cond[which(change==0)] <- 5

# Prepare final data set to use for our analysis
data <- data.frame("sub" = tmp$sub, "cond" = cond, "change" = change, 
                   "change_quality" = tmp$change_quality, "change_type" = tmp$change_type,
                   "response" = tmp$response, "rt" = tmp$rt)

# No. of observations
nrow(data)

######################################################################
# A custom function to compute EZ summary statistics
######################################################################

ez_summaries <- function(data){
  # Identify condition and subject ID
  cond <- unique(data$cond)
  sub <- unique(data$sub)
  # Prepare EZ statistics
  output<- c()
  for(i in sort(sub)){
    # For each subject...
    for(k in sort(cond)){
      # On each condition...
      # Isolate the data
      subset <- data[which(data$sub==i&data$cond==k),]
      # And prepare summary statistics
      output <- rbind(output, c("sub" = unique(subset$sub), "cond" = unique(subset$cond),
                                "change" = unique(subset$change), 
                                "change_quality" = unique(subset$change_quality),
                                "change_type" = unique(subset$change_type),
                                "nTrials" = nrow(subset), "score" = sum(subset$response),
                                "meanRT"  = mean(subset$rt), "varRT"= var(subset$rt)))
    }
  }
  # Return data frame with summary statistics per condition and subject
  return(as.data.frame(output))
}

ezdata <- ez_summaries(data)
ezdata$acc_rate <- ezdata$score/ezdata$nTrials


# Is there a change?
# Yes (1) / No (0)
X <- ezdata$change

# Change quality
# Quantitative (1) / Qualitative (0)
Y <- ezdata$change_quality

# Change type
# Concavity (1) / Convexity (0)
Z <- ezdata$change_type


modelFile <- here("output", "BUGS-models", "demo_shape_model.bug")
model <- write("
    model {
              ####### Priors
              drift_mu ~ dnorm(0,1)   # Baseline
              drift_lambda ~ dgamma(2,1)
              drift_sigma = pow(drift_lambda, -0.5)
              for(i in 1:4){
                  gamma[i] ~ dnorm(0,1)   
              }      
              
              for(j in 1:5){
                  drift_pred[j] = drift_mu + X[j]*(gamma[1]*Y[j]+gamma[2]*Z[j]+gamma[3]*Y[j]*Z[j]) + (1-X[j])*gamma[4]
              }
              
              ####### Sampling model
              for (k in 1:length(nTrials)) {
                  # Person-by-condition parameters for DM parameters
                  bound[k] ~ dgamma(2,1)
                  nondt[k] ~ dexp(1)
                  drift[k] ~ dnorm(drift_pred[cond[k]],drift_lambda)
          
                  # Forward equations from EZ Diffusion
                  ey[k]  = exp(-bound[k] * drift[k])
                  Pc[k]  = 1 / (1 + ey[k])
                  PRT[k] = 2 * pow(drift[k], 3) / bound[k] * pow(ey[k] + 1, 2) / (2 * -bound[k] * 
                           drift[k] * ey[k] - ey[k] * ey[k] + 1)
                  MDT[k] = (bound[k] / (2 * drift[k])) * (1 - ey[k]) / (1 + ey[k])
                  MRT[k] = MDT[k] + nondt[k]
          
                  # Sampling distributions for summary statistics
                  correct[k] ~ dbin(Pc[k], nTrials[k])
                  varRT[k]   ~ dnorm(1/PRT[k], 0.5*(nTrials[k]-1) * PRT[k] * PRT[k])
                  meanRT[k]  ~ dnorm(MRT[k], PRT[k] * nTrials[k])
                }
    }", modelFile)


    # General setup
n.chains  <- 4;      n.iter    <- 2500
n.burnin  <- 250;    n.thin    <- 1

# Pass data to JAGS
data_toJAGS <- list("nTrials"  =  ezdata$nTrials,
                    "meanRT"   =  ezdata$meanRT,
                    "varRT"    =  ezdata$varRT,
                    "correct"  =  ezdata$score,
                    "cond"     =  ezdata$cond,
                    "X" = X, "Y" = Y, "Z" = Z)

# Specify parameters to keep track of
parameters <- c('gamma', 'drift_mu', 'drift_lambda', 'drift_sigma', 'drift_pred',
                'drift','bound', 'nondt', "Pc", "PRT", "MRT")

set.seed(seed)

# Prepare initial values
myinits <- rep(list(list()), n.chains)
for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nrow(ezdata),0,1))
}

set.seed(seed)

start <- Sys.time()
samples <- jags(data=data_toJAGS,
                parameters.to.save=parameters,
                model=modelFile,
                n.chains=n.chains,  n.iter=1000,
                n.burnin=100,  n.thin=n.thin,
                DIC=T, inits=myinits)
end <- Sys.time()

rhats <- apply(samples$BUGSoutput$sims.array,3,getRhat)
rule <- 1.05
bad.Rhat <- which(rhats>rule)
test.rhat <- length(bad.Rhat) > 0
  if(test.rhat){
          par(mfrow=c(1,1))
          which.are.bad.Rhats <- names(bad.Rhat)
          hist(rhats, breaks = 50)
          abline(v=rule, col="red", lty=2)
          legend("top",paste("Rhat > ",rule," | ",
                             (round(nrow(bad.Rhat)/(length(as.vector(rhats))),5))*100,
                             "% of chains | ", length(which.are.bad.Rhats), " chains", sep=""), lty=2, col="red", cex=0.4)
          table(which.are.bad.Rhats)
  }else{      paste("No Rhat greater than ", rule, sep="")       }

  ##### Drift rate parameters
# Recovered drift rates
drift <- samples$BUGSoutput$sims.list$drift
# Regression coefficients
mu <- samples$BUGSoutput$sims.list$drift_mu
gamma <- samples$BUGSoutput$sims.list$gamma
# Fitted values / Predicted drift rates
drift_pred <- samples$BUGSoutput$sims.list$drift_pred



epsilon <- 0.1
prior_constant <- pnorm(epsilon) - pnorm(-epsilon)
for(i in 1:3){
    g <- gamma[,i]
    post_mass <- mean(g > -epsilon & g < epsilon)
    this.BF <- prior_constant/post_mass
    this.BF[post_mass==0] <- 0
    cat("The B.F. for gamma", i, "in favor of an effect is", this.BF, "\n")
}


##### Summary statistics computed from the recovered drift and boundary
Pc   <- samples$BUGSoutput$sims.list$Pc
PRT  <- samples$BUGSoutput$sims.list$PRT
MRT  <- samples$BUGSoutput$sims.list$MRT


##### Drift rate parameters
# Recovered drift rates
drift <- samples$BUGSoutput$sims.list$drift
# Regression coefficients
mu <- samples$BUGSoutput$sims.list$drift_mu
gamma <- samples$BUGSoutput$sims.list$gamma
# Fitted values / Predicted drift rates
drift_pred <- samples$BUGSoutput$sims.list$drift_pred

##### Summary statistics computed from the recovered drift and boundary
Pc   <- samples$BUGSoutput$sims.list$Pc
PRT  <- samples$BUGSoutput$sims.list$PRT
MRT  <- samples$BUGSoutput$sims.list$MRT


# Custom function to select colors
myCol <- function(r,g,b,sub,alpha=1){
       rgb(r[sub]/255,g[sub]/255,b[sub]/255,alpha)
}

r <- matrix(NA, ncol=5,nrow=9)
g <- matrix(NA, ncol=5,nrow=9)
b <- matrix(NA, ncol=5,nrow=9)

# Paleta violeta
r[,1] <- round(seq(124,234, length.out=9),0)
g[,1] <- round(seq(33,46, length.out=9),0)
b[,1] <- round(seq(135,255, length.out=9),0)

# Paleta verde
r[,2] <- round(seq(30,90, length.out=9),0)
g[,2] <- round(seq(116,255, length.out=9),0)
b[,2] <- round(seq(54,93, length.out=9),0)

# Paleta azul
r[,3] <- round(seq(44,108, length.out=9),0)
g[,3] <- round(seq(87,173, length.out=9),0)
b[,3] <- round(seq(142,255, length.out=9),0)

# Paleta amarillo
r[,4] <- round(seq(123,255, length.out=9),0)
g[,4] <- round(seq(104,210, length.out=9),0)
b[,4] <- round(seq(34,49, length.out=9),0)

# Paleta rojo
r[,5] <- round(seq(99,255, length.out=9),0)
g[,5] <- round(seq(25,40, length.out=9),0)
b[,5] <- round(seq(25,40, length.out=9),0)