####################
# Setup
####################
cat("\n\n===== SETUP =====\n")
cat("Setting up environment and loading required packages...\n")
library(R2jags)
library(here)
set.seed(15)
source(here("src", "JAGS_getRhat.R"))

####################
# Loading data
####################
cat("\n\n===== DATA LOADING =====\n")
cat("Loading participant data...\n")
# Load the data from one participant
data_raw <- read.csv(here("demos", "applications", "brightness_perception", "data", "nh.tsv"), sep = "")
colnames(data_raw) <- c("index","cond","response","RT")
head(data_raw)

# No. of observations
cat(sprintf("\nLoaded %d observations from participant data\n", nrow(data_raw)))

####################
# Data preprocessing
####################
cat("\n\n===== DATA PREPROCESSING =====\n")
cat("Preprocessing data...\n")
# Create a copy of the raw data file 
data <- data_raw

# Get 'accuracy' binary coding based on the condition and response in raw data file
# Conditions 1-16 and 34-49: correct response is 1
# Conditions 18-33 and 51-66: correct response is 2
accuracy <- as.integer(data_raw$cond > 0  & data_raw$cond < 17 & data_raw$response==1|
                       data_raw$cond > 17 & data_raw$cond < 34 & data_raw$response==2|
                       data_raw$cond > 33 & data_raw$cond < 50 & data_raw$response==1|
                       data_raw$cond > 50 & data_raw$cond < 67 & data_raw$response==2)

# Update the 'response' column of the data copied so it reflects accuracy
data$response <- accuracy

# Remove rows where RT > 3000ms
removed_count <- nrow(data_raw) - nrow(data[which(data$RT<=3000),])
data <- data[which(data$RT<=3000),]
cat(sprintf("\nRemoved %d trials with RT > 3000ms, %d observations remain\n", 
            removed_count, nrow(data)))

####################
# Computing summary statistics
####################
cat("\n\n===== SUMMARY STATISTICS =====\n")
cat("Computing summary statistics for EZ-DDM...\n")

# Define a function to compute the summary statistics used by EZ-DDM
ez_summaries <- function(subset){
  # Identify condition ID
  cond <- unique(subset$cond)
  # Return relevant summary statistics
  return(data.frame("nTrials" = nrow(subset),
                    "score" = sum(subset$response),
                    "meanRT"  = mean(subset$RT/1000),
                    "varRT"   = var(subset$RT/1000),
                  # Index variable: Accuracy (-0.5) vs Speed (0.5) condition
                    "Xi"  = as.integer(cond>33)-0.5,
                  # Arbitrary scale of stimulus configuration | 0 is 50/50 black and white 
                    "Xs"  = ((cond-1) %% 33 - 16)/5))
}

# Initialize an empty output data frame (df)
tmp <- matrix(0,nrow = max(data$cond),ncol = 6)
df <- as.data.frame(tmp)
colnames(df) <- c("nTrials", "sum_accuracy", "mean_rt",
                  "variance_rt", "Xi", "Xs")

# Populate the df output using the ez_summaries function
cat("\nAggregating data by condition...\n")
for(i in 1:max(data$cond)){
  df[i,] <- ez_summaries(data[which(data$cond==i),])
}

# Remove the two ambiguous conditions (17 and 50, with 50/50 black and white)
df <- df[-which(df$Xs==0),]
cat("\nRemoved ambiguous conditions (50/50 black and white)\n")
head(df,3)

# Compute accuracy rate per condition from the summary data
df$acc_rate <- df$sum_accuracy/df$nTrials

####################
# Defining the JAGS model
####################
cat("\n\n===== MODEL DEFINITION =====\n")
cat("Preparing JAGS model file...\n")

JAGS_model_file <- here("output", "BUGS-models", "demo_brightness_model.bug")
if(!file.exists(JAGS_model_file)){
    model <- write("
                        model {
                                ##### Priors for hierarchical DDM parameters
                                betaweight ~ dnorm(0.00, 1.00)
                                beta0 ~ dnorm(0.00, 1.00)
                                beta1 ~ dnorm(0.00, 1.00)
                                beta2 ~ dnorm(0.00, 1.00)
                                beta3 ~ dnorm(0.00, 1.00)
                                beta4 ~ dnorm(0.00, 1.00)
                                bound_mean ~ dnorm(1.50, (0.20^-2))T( 0.10, 3.00)
                                drift_mean ~ dnorm(0.50, (0.50^-2))
                                nondt_mean ~ dnorm(0.30, (0.06^-2))T( 0, )
                                bound_sdev ~ dunif(0.01, 1.00)
                                drift_sdev ~ dunif(0.01, 3.00)
                                nondt_sdev ~ dunif(0.01, 0.50)
                                
                                # Hierarchical distributions of individual DDM parameters.        
                                for (p in 1:length(meanRT)) {
                                    # Here, we focus on the drift rate
                                    drift_pred[p] = beta0*phi(beta1 + beta2*abs(Xs[p]) + beta3*Xi[p]*abs(Xs[p])) + beta4 * Xi[p] + drift_mean
                                    drift[p] ~ dnorm(drift_pred[p], (drift_sdev^-2))
                                    bound_pred[p] = bound_mean + betaweight * Xi[p]
                                    bound[p] ~ dnorm(bound_pred[p],(bound_sdev^-2))T( 0.10, 3.00)
                                    nondt[p] ~ dnorm(nondt_mean, (nondt_sdev^-2))T( 0.05, )
                                
                                    # Forward equations from EZ DDM
                                    ey[p]  = exp(-bound[p] * drift[p])
                                    Pc[p]  = 1 / (1 + ey[p])
                                    PRT[p] = 2 * pow(drift[p], 3) / bound[p] * 
                                            pow(ey[p] + 1, 2) / (2 * -bound[p] * 
                                            drift[p] * ey[p] - ey[p] * ey[p] + 1)
                                    MDT[p] = (bound[p] / (2 * drift[p])) * 
                                            (1 - ey[p]) / (1 + ey[p])
                                    MRT[p] = MDT[p] + nondt[p]
                                    
                                    # Noiseless predictions from forward EZ DDM
                                    ey_pred[p]  = exp(-bound_pred[p] * drift_pred[p])
                                    Pc_pred[p]  = 1 / (1 + ey_pred[p])
                                    PRT_pred[p] = 2 * pow(drift_pred[p], 3) / bound_pred[p] * 
                                            pow(ey_pred[p] + 1, 2) / (2 * -bound_pred[p] * 
                                            drift_pred[p] * ey_pred[p] - ey_pred[p] * ey_pred[p] + 1)
                                    MDT_pred[p] = (bound_pred[p] / (2 * drift_pred[p])) * 
                                            (1 - ey_pred[p]) / (1 + ey_pred[p])
                                    MRT_pred[p] = MDT_pred[p] + nondt_mean
                                
                                    # Sampling distributions for summary statistics
                                    correct[p] ~ dbin(Pc[p], nTrials[p])
                                    varRT[p]   ~ dnorm(1/PRT[p], 0.5*(nTrials[p]-1) 
                                                                * PRT[p] * PRT[p])
                                    meanRT[p]  ~ dnorm(MRT[p], PRT[p] * nTrials[p])
                            }
                        }", JAGS_model_file)
    cat(sprintf("\nJAGS model file created at %s\n", JAGS_model_file))
}

####################
# Setting up JAGS parameters
####################
cat("\n\n===== MODEL SETUP =====\n")
cat("Setting up JAGS parameters...\n")
# General setup
n.chains  <- 4;      n.iter    <- 5000
n.burnin  <- 250;    n.thin    <- 1

# Pass data to JAGS
data_toJAGS <- list("nTrials"  =  df$nTrials,
                    "meanRT"   =  df$mean_rt,
                    "varRT"    =  df$variance_rt,
                    "correct"  =  df$sum_accuracy,
                    "Xi"   =  df$Xi, "Xs"   =  df$Xs)

# Specify parameters to keep track of
parameters <- c('beta3', 'beta4', 'drift', 'drift_pred',
                "Pc_pred", "MRT_pred", "PRT_pred", "Pc", "PRT", "MRT")

# Prepare initial values
cat("\nPreparing initial values for MCMC chains...\n")
myinits <- rep(list(list()), n.chains)
for(i in 1:n.chains){
    myinits[[i]] <- list(drift = rnorm(length(data_toJAGS$nTrials),0,0.1))
}

####################
# Running JAGS model
####################
cat("\n\n===== MODEL FITTING =====\n")
cat("Running JAGS model (this may take a while)...\n")

start <- Sys.time()
samples <- jags(data=data_toJAGS,
                parameters.to.save=parameters,
                model=JAGS_model_file,
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
##### Drift rate parameters
# Recovered drift rates
drift <- samples$BUGSoutput$sims.list$drift
# Effects of instruction
beta3 <- as.vector(samples$BUGSoutput$sims.list$beta3) # Main
beta4 <- samples$BUGSoutput$sims.list$beta4 # Interaction
# Fitted values / Predicted drift rates
drift_pred <- samples$BUGSoutput$sims.list$drift_pred

##### Summary statistics predicted from the predicted drift and boundary
accRate_hat   <- samples$BUGSoutput$sims.list$Pc_pred
rtMean_hat <- samples$BUGSoutput$sims.list$MRT_pred
rtVar_hat  <- 1/samples$BUGSoutput$sims.list$PRT_pred

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
nTrials <- df$nTrials
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
# Preparing for plotting
####################
cat("\n\n===== PLOTTING PREPARATION =====\n")
cat("Preparing data for plotting...\n")
# Identify conditions to plot on x axis
x_values <- unique(df$Xs)
# Insert a 'jump' in between these values
fit_x <- c(x_values[1:16],NA,x_values[17:32])
full_x <- c(fit_x,NA,fit_x+6.8)

####################
# Saving all workspace objects
##########################
cat("\n✓ Analysis complete! \n")

cat("\n\n===== SAVING RESULTS =====\n")
cat("Saving all workspace objects to a single file...\n")

save_workspace_to <- here("output", "RData-results", "demo_brightness_results.RData")
# Save only specific objects
save(samples, df, drift, beta3, beta4, file=save_workspace_to)

cat(sprintf("\n✓ All workspace objects have been saved to %s\n", save_workspace_to), "\n")
