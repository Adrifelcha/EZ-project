library(here)

### Load all function scripts nested in the ./code/functions/ directory
# Call the function within the src directory
source(here("src", "loading", "load_allFunctions.R"))
load_allCustomFunctions()

## General design 
cat("This script is used to run a short simulation study\n")
cat("where we specify a design cell and run the simulation over many iterations (seeds)")
cat("It is used to test the functionality of the simulation study.\n")

### Simulation settings
n.participants <-  40 
n.trials       <- 160
n.simulations  <- 100
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

cat("\n\n========================================\n")
cat("Simulation settings:\n")
cat("========================================\n")
cat("\nModelType =", modelType, "\n\n")
cat("n.participants =", n.participants, "\n")
cat("n.trials =", n.trials, "\n")
cat("n.simulations =", n.simulations, "\n\n")
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


fileName <- nameOutput(nTrials = n.trials, nParticipants = n.participants, nDatasets = n.simulations, 
                       modelType = modelType, fromPrior = fromPrior, output.folder = here("output", "RData-results"))

cat("Output stored in", fileName, "\n")

plot_recovery(simM)

