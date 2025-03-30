####################
# Setup
####################
cat("\n\n===== SETUP =====\n")
cat("Setting up environment and loading required packages...\n")
library(R2jags)
library(here)
source(here("src", "JAGS_getRhat.R"))
set.seed(15)

####################
# Loading data
####################
cat("\n\n===== DATA LOADING =====\n")
cat("Loading shape perception data...\n")
data_raw <- read.csv(here("demos", "applications", "shape_perception", "data", "vpw08.csv"))
colnames(data_raw) <- c("sub", "change_quality", "change_type", "noChange", "response", "rt")
head(data_raw)

# No. of observations
cat(sprintf("\nLoaded %d observations across %d participants (%d trials per participant)\n", 
            nrow(data_raw), length(unique(data_raw$sub)), unique(table(data_raw$sub))))

####################
# Data preprocessing
####################
cat("\n\n===== DATA PREPROCESSING =====\n")
cat("Preprocessing data...\n")
# Remove RTs larger than 3 seconds and store in a temporary dataframe
tmp <- data_raw[which(data_raw$rt<=3),]

# Add a condition index
change <- 1-tmp$noChange
cond <- rep(0,nrow(tmp))
cond[which(tmp$change_quality==0&tmp$change_type==0)] <- 1  # Qualitative, Convexity
cond[which(tmp$change_quality==1&tmp$change_type==0)] <- 2  # Quantitative, Convexity
cond[which(tmp$change_quality==0&tmp$change_type==1)] <- 3  # Qualitative, Concavity
cond[which(tmp$change_quality==1&tmp$change_type==1)] <- 4  # Quantitative, Concavity
cond[which(change==0)] <- 5                                 # No change

# Prepare final dataset
data <- data.frame("sub" = tmp$sub, "cond" = cond, "change" = change, 
                   "change_quality" = tmp$change_quality, "change_type" = tmp$change_type,
                   "response" = tmp$response, "rt" = tmp$rt)

cat(sprintf("\nFinal Total: %d observations across %d participants (between %d - %d trials per participant)\n", 
            nrow(data), length(unique(data$sub)), min(table(data$sub)), max(table(data$sub))))

####################
# Computing summary statistics
####################
cat("\n\n===== SUMMARY STATISTICS =====\n")
cat("Computing summary statistics for EZ-DDM...\n")

# Define a function to compute the summary statistics used by EZ-DDM
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

cat("\nAggregating data by condition and subject...\n")
ezdata <- ez_summaries(data)
ezdata$acc_rate <- ezdata$score/ezdata$nTrials

####################
# Defining the JAGS model
####################
cat("\n\n===== MODEL DEFINITION =====\n")
cat("Preparing JAGS model file...\n")

modelFile <- here("output", "BUGS-models", "demo_shape_model.bug")
if(!file.exists(modelFile)){
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
                                drift_pred[j] = drift_mu + A[j]*(gamma[1]*B[j]+gamma[2]*C[j]+gamma[3]*B[j]*C[j]) + (1-A[j])*gamma[4]
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

    cat(sprintf("\nJAGS model file created at %s\n", modelFile))
}

####################
# Setting up JAGS parameters
####################
cat("\n\n===== MODEL SETUP =====\n")
cat("Setting up JAGS parameters...\n")
# General setup for MCMC sampling
n.chains  <- 4;     n.iter    <- 2500
n.burnin  <- 250;   n.thin    <- 1

# Pass data to JAGS
data_toJAGS <- list("nTrials"  =  ezdata$nTrials,
                    "meanRT"   =  ezdata$meanRT,
                    "varRT"    =  ezdata$varRT,
                    "correct"  =  ezdata$score,
                    "cond"     =  ezdata$cond,
                    "A" = ezdata$change, # Is there a change? (Yes = 1 / No = 0)
                    "B" = ezdata$change_quality, # Change quality (Quantitative = 1 / Qualitative = 0)
                    "C" = ezdata$change_type) # Change type (Concavity = 1 / Convexity = 0)

# Specify parameters to keep track of
parameters <- c('gamma', 'drift_mu', 'drift_lambda', 'drift_sigma', 'drift_pred',
                'drift','bound', 'nondt', "Pc", "PRT", "MRT")

# Prepare initial values
cat("\nPreparing initial values for MCMC chains...\n")
myinits <- rep(list(list()), n.chains)
for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(nrow(ezdata),0,1))
}

####################
# Running JAGS model
####################
cat("\n\n===== MODEL FITTING =====\n")
cat("Running JAGS model (this may take a while)...\n")
start <- Sys.time()
samples <- jags(data=data_toJAGS,
                parameters.to.save=parameters,
                model=modelFile,
                n.chains=n.chains,  n.iter=1000,
                n.burnin=100,  n.thin=n.thin,
                DIC=T, inits=myinits)
end <- Sys.time()
cat(sprintf("\nJAGS model completed in %s\n", format(end - start)))

##########################
# Convergence diagnostics
##########################
cat("\n\n===== CONVERGENCE DIAGNOSTICS =====\n")
cat("Checking MCMC convergence diagnostics...\n")

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
    cat("\n⚠️ WARNING: Some parameters have not converged (Rhat > 1.05)\n")
} else {      
    cat("\n✓ All parameters have converged (Rhat <= 1.05)\n")
}

####################
# Extracting model parameters
####################
cat("\n\n===== PARAMETER EXTRACTION =====\n")
cat("Extracting model parameters...\n")
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

####################
# Generating posterior predictions
####################
cat("\n\n===== POSTERIOR PREDICTIONS =====\n")
cat("Generating posterior predictions...\n")
# Number of trials per condition
nTrials <- ezdata$nTrials
# Number of posterior samples
n <- nrow(Pc) 
# Number of conditions
J <- ncol(Pc)
# Empty matrices to store posterior predictions
pp_accRate <- matrix(NA, nrow=n, ncol=J)
pp_meanRT  <- matrix(NA, nrow=n, ncol=J)
pp_varRT   <- matrix(NA, nrow=n, ncol=J)
# Obtain posterior predictions using sampling distributions
#        and the summary statistics derived from the recovered
#        drift and boundary parameters
for(i in 1:J){
  correct  <-  rbinom(n,nTrials[i],Pc[,i])
  pp_accRate[,i] <- correct/nTrials[i]
  pp_varRT[,i]   <- rnorm(n,1/PRT[,i], sqrt(2/((nTrials[i]-1) * PRT[,i] * PRT[,i])))
  pp_meanRT[,i]  <- rnorm(n,MRT[,i],sqrt(1/(PRT[,i]*nTrials[i])))
}

####################
# Setting up visualization
####################
cat("\n\n===== PLOTTING PREPARATION =====\n")
cat("Preparing plotting variables...\n")

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

####################
# Computing Bayes Factors
####################
cat("\n\n===== BAYES FACTORS =====\n")
cat("Computing Bayes Factors for effects...\n")
epsilon <- 0.1
prior_constant <- pnorm(epsilon) - pnorm(-epsilon)
BayesFactors <- c()
for(i in 1:3){
    this.gamma   <- gamma[,i]
    post_mass <- mean(this.gamma > -epsilon & this.gamma < epsilon)
    this.BF <- prior_constant/post_mass
    this.BF[post_mass==0] <- 0
    BayesFactors <- c(BayesFactors, this.BF)
    cat(sprintf("\nBayes Factor for gamma %d: %.2f\n", i, this.BF))
}

####################
# Saving all workspace objects
####################
cat("\n✓ Analysis complete! \n")

cat("\n\n===== SAVING RESULTS =====\n")
cat("Saving relevant workspace objects to a single file...\n")

save_workspace_to <- here("output", "RData-results", "demo_shape_results.RData")
# Save key analysis objects
save(samples, ezdata, drift, gamma, mu, drift_pred, r, g, b, BayesFactors, pp_accRate, pp_meanRT, pp_varRT, file=save_workspace_to)

cat(sprintf("\n✓ All workspace objects have been saved to %s\n", save_workspace_to), "\n")