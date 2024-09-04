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
        if(par=="nondt_mean"){plot.range <- c(0.2,0.45)}
        if(par=="bound_mean"){plot.range <- c(1,2)}
        if(par=="betaweight"){
              if(grepl("nondt", simStudyRData)){  plot.range <- c(0,1)  
              }else{   plot.range <- c(-1,1)} 
        }
        fixed.axis <- seq(plot.range[1],plot.range[2],length.out=2)
    
        par(pty="s", mfrow=c(5,5), mai=c(0,0,0.05,0.05), oma=c(2,1.5,1.5,1.5))
        for(i in 1:5){
              thisP_x <- obj$true[[i]]
              thisP_y <- obj$recovered[[i]]
          for(k in 1:5){
              x <- thisP_x[,param,k]
              y <- thisP_y[,param,k]
              plot(5,5, xlim=plot.range, ylim=plot.range, ann=F, axes=F, col="#3480C5")
              abline(v=mean(plot.range),lty=3,col="gray60")
              abline(h=mean(plot.range),lty=3,col="gray60")
              #mtext(paste(par), outer = TRUE, line=-20, cex=3, col="gray90")
              points(x,y,col="#3480C5",pch=16)
              box(lty=3)
              abline(0,1,lty=3,lwd=3,col="gray20")
              if(k==1){mtext(paste("P =",P[i]),2,line=0.5,f=2)}
              if(i==1){mtext(paste("T =",Tr[k]),line=0.5,f=2)}
              if(par=="bound_mean"){
                  if(i==5){axis(1,fixed.axis,round(fixed.axis,0))}
                  if(k==5){axis(4,fixed.axis,round(fixed.axis,0), las=2)}
              }else{
                  if(i==5){axis(1,fixed.axis,round(fixed.axis,1))}
                  if(k==5){axis(4,fixed.axis,round(fixed.axis,1), las=2)}
              }
          }
        }
    }
}
