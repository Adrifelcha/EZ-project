plot_recovery <- function(simOutput){
  singleRun <- nrow(simOutput$estimates)==1
  findIndiv_Est <- which(grepl("\\[",colnames(simOutput$estimates)))
  indiv_Est  <- simOutput$estimates[,findIndiv_Est]
  parnt_Est <- simOutput$estimates[,-findIndiv_Est]
  findIndiv_Tru <- which(grepl("\\[",colnames(simOutput$trueValues)))
  indiv_Tru  <- simOutput$trueValues[,findIndiv_Tru]
  parnt_Tru  <- simOutput$trueValues[,-findIndiv_Tru]
  if(!singleRun){
    colorPar <- c("#AB1914", "#42AB14", "#F8B51A", "#44E5A3", 
                  "#4483E5", "#9044E5", "#E544BE", "#0144A4")
    findParents_Est <- which(grepl("\\_",colnames(parnt_Est)))
    findParents_Tru <- which(grepl("\\_",colnames(parnt_Tru)))
    parBeta_Est <- parnt_Est[,-findParents_Est]
    parBeta_Tru <- parnt_Tru[,-findParents_Tru]
    par(mfrow = c(1,1))
        plot.range <- c(min(parBeta_Tru,parBeta_Est),max(parBeta_Tru,parBeta_Est))
        plot(parBeta_Tru,parBeta_Est, ann=F, col=colorPar[length(colorPar)], pch=16,
             xlim=plot.range, ylim=plot.range)
        abline(0,1, lty=2)
        mtext("Simulated values", 1, line=2.1, cex=0.7)
        mtext(colnames(parnt_Est)[-findParents_Est], 3, line=0.5, cex=1, f=2)
    parnt_Est <- parnt_Est[,findParents_Est]
    parnt_Tru <- parnt_Tru[,findParents_Tru]
    idParPar <- colnames(parnt_Est)
    nParPar  <- length(idParPar)
    par(mfrow = c(2, nParPar/2))
    for(p in 1:nParPar){
        x.true <- parnt_Tru[,which(grepl(idParPar[p],colnames(parnt_Tru)))]
        y.esti <- parnt_Est[,which(grepl(idParPar[p],colnames(parnt_Est)))]
        plot.range <- c(min(x.true,y.esti),max(x.true,y.esti))
        plot(x.true,y.esti, ann=F, col=colorPar[p], pch=16,
             xlim=plot.range, ylim=plot.range)
        abline(0,1, lty=2)
        mtext("Simulated values", 1, line=2.1, cex=0.7)
        mtext(idParPar[p], 3, line=0.5, cex=0.8, f=2)
    }
    idIndPar <- unique(sub("\\[.*", "", colnames(indiv_Est)))
    colorInd <- c(rgb(33/255,178/255,81/255,0.5),
                  rgb(33/255,77/255,178/255,0.5),
                  rgb(178/255,33/255,143/255,0.5),
                  rgb(220/255,172/255,18/255,0.5))
    par(mfrow = c(1, length(idIndPar)), mar =c(5.1,2,4.1,2))
    for(p in 1:length(idIndPar)){
      x.true <- indiv_Tru[,which(grepl(idIndPar[p],colnames(indiv_Tru)))]
      y.esti <- indiv_Est[,which(grepl(idIndPar[p],colnames(indiv_Est)))]
      plot.range <- c(min(x.true,y.esti),max(x.true,y.esti))
      plot(x.true,y.esti, ann=F, col=colorInd[p], pch=16, cex=0.7,
           xlim=plot.range, ylim=plot.range)
      abline(0,1, lty=2)
      mtext("Simulated values", 1, line=2.1, cex=0.7)
      mtext(paste("Individual", idIndPar[p]), 3, line=0.5, cex=0.8, f=2)
    }
  }else{
    colorInd <- c("#21B251","#214DB2","#B2218F","#DCAB12")
    idIndPar <- unique(sub("\\[.*", "", names(indiv_Est)))
    par(mfrow = c(1, length(idIndPar)), mar =c(5.1,2,4.1,2))
    for(p in 1:length(idIndPar)){
      x.true <- indiv_Tru[which(grepl(idIndPar[p],names(indiv_Tru)))]
      y.esti <- indiv_Est[which(grepl(idIndPar[p],names(indiv_Est)))]
      plot.range <- c(min(x.true,y.esti),max(x.true,y.esti))
      plot(x.true,y.esti, ann=F, col=colorInd[p], pch=16,
           xlim=plot.range, ylim=plot.range)
      abline(0,1, lty=2)
      mtext("Simulated values", 1, line=2.1, cex=0.7)
      mtext(paste("Individual", idIndPar[p]), 3, line=0.5, cex=0.8, f=2)
    }
  }
}