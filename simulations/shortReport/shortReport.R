# Short report illustrating the recovery of the hierarchical EZ-DDM parameters

## Preamble

### Load packages
library(rmdformats)
library(here)

### Load all function scripts nested in the ./code/functions/ directory
for(archive in dir(here("code", "functions/"))) {   
   source(here("code", "functions", archive))     
   }

### Load the higher-level functions that call individual functions to do specific routines
source(here("code", "scripts", "HDDM_setup.R"))      # Sample parameters and generate data
source(here("code", "scripts", "HDDM_runJAGS.R"))    # Run JAGS
source(here("code", "scripts", "HDDM_runSims.R"))    # Run full simulation study


## General design 

### Simulation settings
n.participants <-  40 
n.trials       <- 160
n.simulations  <- 100
modelType      <- "metaregression"

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
store_results_to     <- here("simulations", "short_report")


## Run the simulation

simM <- HDDM_runSims(nParticipants = n.participants, 
                     nTrials = n.trials, 
                     nDatasets = n.simulations, 
                     n.iter = n.iter, 
                     n.chains = n.chains, 
                     n.burnin = n.burnin, 
                     n.thin = n.thin,
                     fromPrior = fromPrior, 
                     output.folder = store_results_to, 
                     Show = Show,
                     track_allParameters = track_all_parameters,
                     rhatCheck = rhatCheck,
                     redo_if_bad_rhat = redo_if_bad_rhat,
                     modelType = modelType)

plot_recovery(simM)

