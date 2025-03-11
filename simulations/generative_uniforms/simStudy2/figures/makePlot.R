#source("./plot_simStudyOutput_outdated.R")
source("../../../../code/functions/plot_simStudyOutput.R")
png(file = "./simMeta_drift_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", par="drift_mean")
dev.off()

png(file = "./simMeta_bound_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", par="drift_mean")
dev.off()

png(file = "./simMeta_nondt_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", par="drift_mean")
dev.off()

####################

png(file = "./simMeta_drift_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", par="nondt_mean")
dev.off()

png(file = "./simMeta_bound_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", par="nondt_mean")
dev.off()

png(file = "./simMeta_nondt_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", par="nondt_mean")
dev.off()

####################

png(file = "./simMeta_drift_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", par="bound_mean")
dev.off()

png(file = "./simMeta_bound_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", par="bound_mean")
dev.off()

png(file = "./simMeta_nondt_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", par="bound_mean")
dev.off()

####################

png(file = "./simMeta_drift_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_drift.RData", par="betaweight")
dev.off()

png(file = "./simMeta_bound_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_bound.RData", par="betaweight")
dev.off()

png(file = "./simMeta_nondt_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Meta_nondt.RData", par="betaweight")
dev.off()


#########################33
############################
###############################


png(file = "./simTtest_drift_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", par="drift_mean")
dev.off()

png(file = "./simTtest_bound_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", par="drift_mean")
dev.off()

png(file = "./simTtest_nondt_drift.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", par="drift_mean")
dev.off()

####################

png(file = "./simTtest_drift_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", par="nondt_mean")
dev.off()

png(file = "./simTtest_bound_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", par="nondt_mean")
dev.off()

png(file = "./simTtest_nondt_nondt.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", par="nondt_mean")
dev.off()

####################

png(file = "./simTtest_drift_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", par="bound_mean")
dev.off()

png(file = "./simTtest_bound_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", par="bound_mean")
dev.off()

png(file = "./simTtest_nondt_bound.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", par="bound_mean")
dev.off()

####################

png(file = "./simTtest_drift_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_drift.RData", par="betaweight")
dev.off()

png(file = "./simTtest_bound_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_bound.RData", par="betaweight")
dev.off()

png(file = "./simTtest_nondt_beta.png", width = 5, height = 5, units="in",res=300) # Width and height of the plot in inches
par(bg=NA)
makeSimStudyPlot("../results/simStudy_Ttest_nondt.RData", par="betaweight")
dev.off()
