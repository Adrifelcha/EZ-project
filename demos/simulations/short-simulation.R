library(here)

### Load all function scripts nested in the ./code/functions/ directory
for(archive in dir(here("src"))) {   
   # Skip README file
   if(grepl("README", archive, ignore.case = TRUE)){ 
      next 
    }
   source(here("src", archive))     
}

## General design 

### Simulation settings
n.participants <-  40 
n.trials       <- 160
n.simulations  <- 10
modelType      <- "ttest"

### JAGS variables 
n.iter   <- 300
n.chains <-   4
n.burnin <- 200
n.thin   <-   1

### Preferences for running the simulation 
fromPrior <- TRUE   # Generate data from the prior?
Show      <- FALSE  # Show additional output?
rhatCheck <- TRUE   # Check the Rhats?
redo_if_bad_rhat     <- TRUE # Redo iterations with a bad Rhat?
track_all_parameters <- FALSE # Track all parameters?


cat("Simulation settings:\n")
cat("========================================\n")
cat("n.participants =", n.participants, "\n")
cat("n.trials =", n.trials, "\n")
cat("n.simulations =", n.simulations, "\n")
cat("modelType =", modelType, "\n")
cat("fromPrior =", fromPrior, "\n")
cat("redo_if_bad_rhat =", redo_if_bad_rhat, "\n")
cat("track_all_parameters =", track_all_parameters, "\n\n\n")


cat("Running the simulation...\n\n\n")

## Run the simulation
simM <- HDDM_runSims(nParticipants = n.participants, 
                     nTrials = n.trials, 
                     nDatasets = n.simulations, 
                     n.iter = n.iter, 
                     n.chains = n.chains, 
                     n.burnin = n.burnin, 
                     n.thin = n.thin,
                     fromPrior = fromPrior, 
                     Show = Show,
                     track_allParameters = track_all_parameters,
                     rhatCheck = rhatCheck,
                     redo_if_bad_rhat = redo_if_bad_rhat,
                     modelType = modelType,
                     forceSim = TRUE)

plot_recovery(simM)

