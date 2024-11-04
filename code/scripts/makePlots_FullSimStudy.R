#####################################################################################
#
#####################################################################################
# CUSTOMIZABLE SETTINGS
#simulationDirectory <- "../../simulations/generative_priors/"
#setwd(simulationDirectory)
simStudyID <- "sim"
showParam = TRUE
plotType = 2
h = 6 #height in inches
w = h #width in inches
res = 300
units <- "in"

plot.ranges <- rbind(c(,),   # drift
                     c(,),   # bound
                     c(,),   # nondt
                     c(,))   # betaweight


###############################################################################
# Make plots
###############################################################################
results.at <- "./results/"
results.available <- gsub(".RData","",sub('simStudy_', '', dir(results.at)))
split_ <- strsplit(results.available, "_")
designs.available <- unique(sapply(split_, `[`, 1))
criterion.parameters <- unique(sapply(split_, `[`, 2))
parameters <- cbind(c("drift","bound","nondt","beta"),
                    c("drift_mean","bound_mean","nondt_mean","betaweight"))

for(d in designs.available){
    
    for(c in criterion.parameters){
      
        results.file <- paste("simStudy_",d,"_",c,".RData", sep="")
        simResults <- paste(results.at,results.file,sep="")
        for(i in 1:nrow(parameters)){
            makePNG <- paste("./figures/",simStudyID,d,"_",c,"_",parameters[i,1],".png",sep="")
            png(file = makePNG, width = w, height = h, units=units,res=res) 
            par(bg=NA)
            makeSimStudyPlot(simResults, param=parameters[i,2], plotType=plotType, showParam=showParam)
            dev.off()
        }
      
    }
  
}

