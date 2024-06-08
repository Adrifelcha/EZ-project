plot_multipleRecoveryPlots <- function(simStudy, nParticipants, nTrials){
  par(mfrow = c(2, 3))#, mar =c(5.1,2,4.1,2))
  x <- simStudy$sim_means
  recoveryPlot(x[,"true","drift_mean"],x[,"est","drift_mean"],"Group mean drift")
  recoveryPlot(x[,"true","bound_mean"],x[,"est","bound_mean"],"Group mean bound")
  recoveryPlot(x[,"true","nondt_mean"],x[,"est","nondt_mean"],"Group mean nondt")
  y <- simStudy$sim_indiv
  recoveryPlot(y[,"true","drift"],y[,"est","drift"],"Individual drifts")
  recoveryPlot(y[,"true","bound"],y[,"est","bound"],"Individual bounds")
  recoveryPlot(y[,"true","nondt"],y[,"est","nondt"],"Individual nondt")
  title <- paste(nParticipants, " participants, ", nTrials, " trials each")
  mtext(title, side = 3, line =-1.7, outer = TRUE, f=2, col="red3")
}