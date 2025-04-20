##########################################################
# LOAD FUNCTIONS/PACKAGES
##########################################################
######## Load required R packages for parallel processing
library(here)
library(foreach)
library(doParallel)

########| Load required R scripts
skip_scripts <- c("README.md")
cat("Sourcing scripts...\n")
for(archive in dir(here("src"))){   
    if(archive %in% skip_scripts){
        next
    }else{                
        cat(paste("Sourcing:", archive, "\n"))
        source(here("src", archive))        
    }    
}

##########################################################
# SIMULATION SETTINGS
##########################################################
# Create output directory if it doesn't exist
output_dir <- here("demos", "simulation-studies", "generative_priors", "samples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings <- list("fromPrior" = TRUE,  
                 "output.folder" = file.path(output_dir, "/"),  # Ensure trailing slash
                 "participant_levels" = c(20,40,80,160,320),
                 "trial_levels" = c(20,40,80,160,320),
                 "nDatasets" = 1000,
                 "criterion_levels" = c("drift", "nondt", "bound"),
                 "design_levels" = c("ttest","metaregression"),
                 "n.chains" = 3)



# Create a "settings" object that specifies all relevant aspects of the simulation study
#################| Fixed variables

settings <- c(settings,
              list("nCells" = prod(length(settings$participant_levels),length(settings$trial_levels))*(3*2)))

#################| Specific JAGS objects
jagsParameters <- c("bound_mean", "drift_mean", "nondt_mean", "bound", "nondt",
                    "drift_sdev", "nondt_sdev", "bound_sdev", "drift", "betaweight")

jagsInits <- list()
    for(i in settings$participant_levels){
      jagsInits <- c(jagsInits, list(JAGS_inits(settings$n.chains, i, custom_sd = 0.1)))
    }

# Define the model file names depending on the criterion parameter
modelFile <- matrix(c(rep(c("./EZHBDDM_BetaDrift.bug",
                            "./EZHBDDM_BetaNondt.bug",
                            "./EZHBDDM_BetaBound.bug"),2)), 
                    byrow = TRUE, ncol = 3)
settings <- c(settings, list("jagsParameters" = list(jagsParameters, jagsParameters),
                             "modelFile" = modelFile,
                             "priors" = list(default_priors(Show=FALSE, "ttest"), default_priors(Show=FALSE, "metaregression")),
                             "jagsData" = list(data_toJAGS("ttest"), data_toJAGS("metaregression")),
                             "jagsInits" = jagsInits))
#################| Name objects so they can be called more easily
names(settings$jagsParameters) <- settings$design_levels
names(settings$priors) <- settings$design_levels
names(settings$jagsData) <- settings$design_levels
names(settings$jagsInits) <- settings$participant_levels
colnames(settings$modelFile) <- settings$criterion_levels
rownames(settings$modelFile) <- settings$design_levels
########| Write JAGS models


custom_truncation_list <- list(
                "bound_mean" = c(0.1, 3.0),
                "nondt_mean" = c(0.05, ""),
                "drift_mean" = c(-3, 3),
                "bound_sdev" = c(0.01, ""),
                "nondt_sdev" = c(0.01, ""),
                "drift_sdev" = c(0.01, ""),
                "drift" = c(-3, 3),
                "bound" = c(0.1, 3.0),
                "nondt" = c(0.05, ""),
                "betaweight" = c(-3, 3)
        )

for(model in settings$design_levels){
    for(crit in settings$criterion_levels){
        write_JAGSmodel(priors = settings$priors[[2]], modelType = model, 
                        criterion =crit, modelFile = settings$modelFile[model,crit],
                        custom_truncation_list = custom_truncation_list)
    }
}


################################################################
# Define simulation functions
################################################################
cores       <-  detectCores()
my.cluster  <-  makeCluster(cores[1]-3)

registerDoParallel(cl = my.cluster)
#output <- foreach(i = 120:1000, 
#                  .errorhandling = "pass",
#                  .combine = 'rbind'
#                  ) %dopar% {
#                    Z <- HDDM_runFullSeed(seed = i, settings, forceRun = TRUE)
#                  }
stopCluster(cl = my.cluster)

#newoutput <- output
#output <- rbind(output1to119,output)
#output1to119 <- output

source("../../code/functions/store_parallelOutput.R")
#store_parallelOutput(output, settings, saveTo = "./results/")
makeSimStudyPlot("./results/simStudy_Meta_drift.RData", plotType = 2)


