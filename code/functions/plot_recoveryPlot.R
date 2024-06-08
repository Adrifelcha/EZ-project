plot_recoveryPlot <- function(x.true, y.estimated, parameterName){
  allValues <- c(x.true,y.estimated)
  param.is.bound <- length(which(grepl("ound",parameterName)))!=0
  param.is.nondt <- length(which(grepl("no",parameterName)))!=0
  if(param.is.bound){   color <- "blueviolet"
  ax.lim <- c(0.5,2.5)
  }else{if(param.is.nondt){   color <- "dodgerblue4"
  ax.lim <- c(0,0.75)
  }else{  color <- "goldenrod4"
  ax.lim <- c(-2,2)
  }}
  par(bty="o")
  plot(x.true,y.estimated, ann=F, col=color, pch=16,
       xlim=ax.lim, ylim=ax.lim)
  abline(0,1, lty=2)
  mtext("Simulated values", 1, line=2.1, cex=0.7)
  mtext(parameterName, 3, line=0.5, cex=0.8, f=2)
}