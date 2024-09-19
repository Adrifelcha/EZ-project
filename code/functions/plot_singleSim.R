plot_recovery <- function(simOutput, plotType=1){
      findIndiv_Est <- which(grepl("\\[",colnames(simOutput$estimates)))
      indiv_Est  <- simOutput$estimates[,findIndiv_Est]
      parnt_Est <- simOutput$estimates[,-findIndiv_Est]
      findIndiv_Tru <- which(grepl("\\[",colnames(simOutput$trueValues)))
      indiv_Tru  <- simOutput$trueValues[,findIndiv_Tru]
      parnt_Tru  <- simOutput$trueValues[,-findIndiv_Tru]
      colorPar <- c("#AB1914", "#42AB14", "#F8B51A", "#44E5A3", 
                      "#4483E5", "#9044E5", "#E544BE", "#0144A4")
      idIndPar <- unique(sub("\\[.*", "", colnames(indiv_Est)))
      colorInd <- c(rgb(188/255,56/255,156/255,0.2),
                    rgb(56/255,188/255,58/255,0.2),
                    rgb(247/255,167/255,26/255,0.2))
        if(!is.null(simOutput$settings$X)){
             par(mfrow = c(1,1),mar=c(0,0,3,0))
                  if(plotType==1){
                     make_panel_type1(simOutput, parameter="betaweight", 
                                      add.titles = TRUE, plot.range=NA)
                  }else{
                    make_panel_type2(simOutput, parameter="betaweight", 
                                     add.titles = TRUE, nBins=15, 
                                     plot.range=NA)
                  }
              parnt_Est <- parnt_Est[,findParents_Est]
              parnt_Tru <- parnt_Tru[,findParents_Tru]
        }
        idParPar <- colnames(parnt_Est)
        nParPar  <- length(idParPar)
        
        par(mfrow = c(2,3), oma =c(4,5,0,3), mar=c(2,1,2,2))
        # Parent parameters - use functions
        for(p in c("bound","nondt","drift")){
        # Parent parameters - use functions
              if(plotType==1){
                make_panel_type1(simOutput, parameter=p, 
                                 add.titles = FALSE, plot.range=NA)
              }else{
                make_panel_type2(simOutput, parameter=p, 
                                 add.titles = FALSE, nBins=15, 
                                 plot.range=NA)
              }
        }
        mtext("GROUP MEAN", 4, line=2, cex=1.5, f=2)
        k <- 1
        # Individual parameters - do it here
        for(p in c("bound","nondt","drift")){
        x.true <- indiv_Tru[,which(grepl(p,colnames(indiv_Tru)))]
        y.esti <- indiv_Est[,which(grepl(p,colnames(indiv_Est)))]
        plot.range <- c(min(x.true,y.esti),max(x.true,y.esti))
        plot(x.true,y.esti, ann=F, col=colorInd[k], pch=16, cex=0.7,
               xlim=plot.range, ylim=plot.range, axes=F)
        abline(0,1, lty=2)
        axis(1, seq(min(x.true,y.esti),max(x.true,y.esti),length.out=7),
             round(seq(min(x.true,y.esti),max(x.true,y.esti),length.out=7),1))
        axis(2, seq(min(x.true,y.esti),max(x.true,y.esti),length.out=7),
             round(seq(min(x.true,y.esti),max(x.true,y.esti),length.out=7),1),
             las=2)
        mtext(paste(p), 3, line=0, cex=1, f=2)  
        k <- k+1
        }
        mtext("INDIVIDUAL", 4, line=2, cex=1.5, f=2)
        mtext("Simulated values", 1, line=2, cex=1.5, outer=TRUE, f=2)
        mtext("Retrieved values", 2, line=2, cex=1.5, outer=TRUE, f=2)
} 

plot_recovery(simOutput,1)