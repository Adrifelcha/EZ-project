source("../../../code/functions/plot_simStudyOutput.R")

png(file = "./simMeta_drift_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_bound_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_nondt_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simMeta_drift_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_bound_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_nondt_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simMeta_drift_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_bound_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_nondt_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simMeta_drift_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_bound_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simMeta_nondt_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()


#########################33
############################
###############################


png(file = "./simTtest_drift_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_bound_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_nondt_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", param="drift_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simTtest_drift_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_bound_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_nondt_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", param="nondt_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simTtest_drift_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_bound_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_nondt_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", param="bound_mean", plotType=2, showParam=FALSE)
dev.off()

####################

png(file = "./simTtest_drift_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_bound_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()

png(file = "./simTtest_nondt_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", param="betaweight", plotType=2, showParam=FALSE)
dev.off()
