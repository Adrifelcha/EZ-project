for(archive in dir("../code/functions/")){    source(paste("../code/functions/",archive,sep=""))     }
for(archive in dir("../code/scripts/")){      source(paste("../code/scripts/",archive,sep=""))     }


design <- c("hierarchical", "ttest", "metaregression")
criterion <- c("drift", "bound", "nondt")
nP <- c(20, 40, 80, 160, 320)
nT <- c(20, 40, 80, 160, 320)

for(p in nP){
    for(t in nT){
        for(d in design){
            for(c in criterion){
                try(HDDM_runSims(nParticipants = p, nTrials = t, nDatasets = 1000,
                             priors = NA, modelType = d, criterion = NA,
                             n.chains = 4, Show = FALSE, forceSim = FALSE, fromPrior=FALSE),
                silent = TRUE)
            }
        }
    }
}