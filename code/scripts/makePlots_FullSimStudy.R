#####################################################################################
#
#####################################################################################
# CUSTOMIZABLE SETTINGS
#simulationDirectory <- "../../simulations/generative_priors/"
#setwd(simulationDirectory)
results.at <- "./results/"
save.to <- "../../../figures/"
simStudyID <- "unifPrior"
showParam = FALSE
showStudy = TRUE
plotType = 2
h = 5 #height in inches
w = h #width in inches
res = 300
units <- "in"
nBins <- 11

##### Simulaion study 1: Generative prior
#plot.ranges <- rbind(c(-1.3,1.3),   # drift
#                     c(1,2),        # bound
#                     c(0.15,0.4),   # nondt
#                     c(-1,1))       # betaweight
###### Simulation study 2: Uniform parameters
plot.ranges <- rbind(c(-5.5,5.5),   # drift
                     c(0.5,4),   # bound
                     c(0.15,0.55),   # nondt
                     c(-1,1))   # betaweight

###############################################################################
# Make plots
###############################################################################
results.available <- gsub(".RData","",sub('simStudy_', '', dir(results.at)))
split_ <- strsplit(results.available, "_")
designs.available <- unique(sapply(split_, `[`, 1))
criterion.parameters <- unique(sapply(split_, `[`, 2))
parameters <- cbind(c("drift","bound","nondt","beta"),
                    c("drift_mean","bound_mean","nondt_mean","betaweight"))

for(d in designs.available){
    
    for(c in criterion.parameters){
        plot.range <- plot.ranges
        if(c == "nondt"){       plot.range[4,] <- c(0,1)            }
        results.file <- paste("simStudy_",d,"_",c,".RData", sep="")
        simResults <- paste(results.at,results.file,sep="")
        for(i in 1:nrow(parameters)){
            makePNG <- paste(save.to,simStudyID,d,"_",c,"_",parameters[i,1],".png",sep="")
            png(file = makePNG, width = w, height = h, units=units,res=res) 
            par(bg=NA)
            makeSimStudyPlot(simResults, param=parameters[i,2], plotType=plotType, showParam=showParam, 
                             plot.range=plot.range[i,], showStudy=showStudy, nBins=nBins)
            dev.off()
            
            makePDF <- paste(save.to,simStudyID,d,"_",c,"_",parameters[i,1],".pdf",sep="")
            if(d=="Meta"&c=="bound"){
                pdf(file = makePDF, width = w, height = h)
                par(bg=NA)
                makeSimStudyPlot(simResults, param=parameters[i,2], plotType=plotType, showParam=FALSE, 
                                 plot.range=plot.range[i,], showStudy=showStudy, nBins=nBins, fixedMargins=FALSE)
                dev.off()
            }else{
                pdf(file = makePDF, width = w, height = h)
                par(bg=NA)
                makeSimStudyPlot(simResults, param=parameters[i,2], plotType=plotType, showParam=showParam, 
                                 plot.range=plot.range[i,], showStudy=showStudy, nBins=nBins, fixedMargins=FALSE)
                dev.off()
            }
        }
    }
}


single_example <- "simStudy_Meta_drift.RData"
par <- "betaweight"
getExample <- paste(results.at,single_example, sep="")

example_info <- gsub(".RData","",sub('simStudy_', '', single_example))
example_split_ <- strsplit(example_info, "_")
example_design <- sapply(example_split_, `[`, 1)
example_criterion <- sapply(example_split_, `[`, 2)
plot.range <- c(-1,1)
makePDF <- paste(save.to,simStudyID,example_design,"_",example_criterion,"_",
                 parameters[which(parameters[,2]==par),1],"_sample.pdf",sep="")
pdf(file = makePDF, width = w, height = h) 
par(bg=NA)
makeSimStudyPlot(getExample, param=par, plotType=plotType, showParam=TRUE, 
                 plot.range=plot.range, showStudy=showStudy, nBins=15, fixedMargins=TRUE)
dev.off()