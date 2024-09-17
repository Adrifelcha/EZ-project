plot_recovery <- function(simOutput){
      findIndiv_Est <- which(grepl("\\[",colnames(simOutput$estimates)))
      indiv_Est  <- simOutput$estimates[,findIndiv_Est]
      parnt_Est <- simOutput$estimates[,-findIndiv_Est]
      findIndiv_Tru <- which(grepl("\\[",colnames(simOutput$trueValues)))
      indiv_Tru  <- simOutput$trueValues[,findIndiv_Tru]
      parnt_Tru  <- simOutput$trueValues[,-findIndiv_Tru]
      colorPar <- c("#AB1914", "#42AB14", "#F8B51A", "#44E5A3", 
                      "#4483E5", "#9044E5", "#E544BE", "#0144A4")
        if(!is.null(simOutput$settings$X)){
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
        }
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
} 


make_panel <- function(simOutput, parameter=NA, add.titles = FALSE, nBins=15){
  if(is.na(parameter)){ stop("Please specify a parameter")  }else{
    if(parameter=="drift"){              parameter <- "drift_mean"
    }else{if(parameter=="bound"){        parameter <- "bound_mean"  
    }else{if(parameter=="nondt"){        parameter <- "nondt_mean"   
    }}}
    
    x <- output$trueValues[,parameter]
    y <- output$estimates[,parameter]
    n <- length(x)
    edges <- range(x)
    bins <- seq(edges[1],edges[2],length.out=nBins)
    edges.plot <- range(c(x,y))
    plot.border <- sd(c(x,y))*0.05
    plot.range <- c(edges.plot[1]-plot.border,edges.plot[2]+plot.border)
    plot(x,y, xlim=plot.range, ylim=plot.range, ann=F, axes=F, col="white")
    
    if(add.titles){
        if(parameter=="betaweight"){
          mtext(expression(paste(beta, " coefficient")),3, line=1, f=2, cex=1.5)
          red <- 0.2 ;  green <- 0.8 ; blue <- 0.6
        }else{
        if(parameter=="drift_mean"){
          mtext(expression(paste("Mean drift rate - ", mu[nu])),3, line=1, f=2, cex=1.5)
          red <- 247/255 ;  green <- 167/255 ; blue <- 26/255
        }else{
        if(parameter=="bound_mean"){
          mtext(expression(paste("Mean boundary - ", mu[alpha])),3, line=1, f=2, cex=1.5)
          red <- 188/255 ;  green <- 56/255 ; blue <- 156/255
        }else{
        if(parameter=="nondt_mean"){
          mtext(expression(paste("Mean nondecision time - ", mu[tau])),3, line=1, f=2, cex=1.5)
          red <- 56/255 ;  green <- 188/255 ; blue <- 58/255
        }else{
          mtext(parameter,3, line=1, f=2, cex=1.5)
        }}}}
    }
    
    
    abline(0,1,col="gray70", lwd=2, lty=2)
    heights <- c()
    mids <- c()
    for(b in 2:length(bins)){
      X.inBin <- x[x<=bins[b]&x>=bins[b-1]]
      Y.inBin <- y[x<=bins[b]&x>=bins[b-1]]
      count <- length(X.inBin)
      whiskers <- quantile(Y.inBin,probs = c(0.025,0.975))
      prop  <- count/n
      lines(c(bins[b-1],bins[b]),rep(whiskers[1],2))
      lines(c(bins[b-1],bins[b]),rep(whiskers[2],2))
      polygon(c(bins[b-1],bins[b],bins[b],bins[b-1]),
              c(whiskers[2],whiskers[2],whiskers[1],whiskers[1]),
              col=rgb(red,green,blue,prop), border = NA)
      heights <- rbind(heights,whiskers)
      mids <- append(mids,median(c(bins[b],bins[b-1])))
    }
    points(mids, heights[,1], pch=16, cex=0.5)
    points(mids, heights[,2], pch=16, cex=0.5)
    lines(mids,heights[,1])
    lines(mids,heights[,2], )
    points(x,y, cex=0.8, pch=16, col=rgb(red,green,blue,0.3))
    axis.labels <- seq(plot.range[1],plot.range[2],length.out=7)
    axis(1, axis.labels, round(axis.labels,1))
    axis(2, axis.labels, round(axis.labels,1), las=2)
    if(add.titles){
       mtext("Simulated values",1, line=2.5, f=2, cex=1.2)
       mtext("Recovered values",2, line=2.75, f=2, cex=1.2)
    }
  }
}