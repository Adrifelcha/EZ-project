makeSimStudyPlot <- function(simStudyRData, param=NA){
  assign('obj', get(load(simStudyRData)))
  lvls <- c(20,40,80,160,320)
  P <- lvls
  Tr <- lvls
  if(is.na(param)){   
        param <- c("drift_mean", "nondt_mean", "bound_mean")    
        if(grepl("Meta", simStudyRData)|grepl("Ttest", simStudyRData)){
           param <- c(param,"betaweight")
        }
  }
  
    for(par in param){
        if(par=="drift_mean"){plot.range <- c(-2,2)}
        if(par=="nondt_mean"){plot.range <- c(0,0.5)}
        if(par=="bound_mean"){plot.range <- c(0,3)}
        if(par=="betaweight"){plot.range <- c(-1,1)}
        fixed.axis <- seq(plot.range[1],plot.range[2],length.out=3)
    
        par(mfrow=c(5,5), mai=c(0,0,0.08,0.08), oma=c(2,2,1.5,1.5))
        for(i in 1:5){
              thisP_x <- obj$true[[i]]
              thisP_y <- obj$recovered[[i]]
          for(k in 1:5){
              x <- thisP_x[,param,k]
              y <- thisP_y[,param,k]
              plot(x,y, xlim=plot.range, ylim=plot.range, ann=F, axes=F, pch=16, col="#3480C5")
              box(lty=3)
              abline(0,1,lty=3,lwd=3,col="gray20")
              if(k==1){mtext(paste("P =",P[i]),2,line=0.5,f=2)}
              if(i==1){mtext(paste("T =",Tr[k]),line=0.5,f=2)}
              if(i==5){axis(1,fixed.axis,fixed.axis)}
              if(k==5){axis(4,fixed.axis,fixed.axis)}
          }
        }
    }
}


