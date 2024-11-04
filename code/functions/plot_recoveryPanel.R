make_panel_type1 <- function(simOutput, parameter=NA, add.titles = FALSE, plot.range=NA,
                             axisX=TRUE, axisY=TRUE){
  if(is.na(parameter)){ stop("Please specify a parameter")  }else{
    if(parameter=="drift"){              parameter <- "drift_mean"
               red <- 247/255 ;  green <- 167/255 ; blue <- 26/255
    }else{if(parameter=="bound"){        parameter <- "bound_mean"  
               red <- 188/255 ;  green <- 56/255 ; blue <- 156/255
    }else{if(parameter=="nondt"){        parameter <- "nondt_mean"
               red <- 56/255 ;  green <- 188/255 ; blue <- 58/255
    }else{  red <- 0.2 ;  green <- 0.8 ; blue <- 0.6    }}}
    
    x <- simOutput$trueValues[,parameter]
    y <- simOutput$estimates[,parameter]
    n <- length(x)
    if(sum(is.na(plot.range))>0){
      plot.range <- range(c(x,y))
    }
    
    plot(x,y, xlim=plot.range, ylim=plot.range, ann=F, axes=F, col="white")
    if(add.titles){
      if(parameter=="betaweight"){
        mtext(expression(paste(beta, " coefficient")),3, line=1, f=2, cex=1.5)
      }else{
        if(parameter=="drift_mean"){
          mtext(expression(paste("Mean drift rate - ", mu[nu])),3, line=1, f=2, cex=1.5)
        }else{
          if(parameter=="bound_mean"){
            mtext(expression(paste("Mean boundary - ", mu[alpha])),3, line=1, f=2, cex=1.5)
          }else{
            if(parameter=="nondt_mean"){
              mtext(expression(paste("Mean nondecision time - ", mu[tau])),3, line=1, f=2, cex=1.5)
            }else{
              mtext(parameter,3, line=1, f=2, cex=1.5)
            }}}}
    }
    abline(0,1,col="gray70", lwd=2, lty=2)
    points(x,y, cex=0.8, pch=16, col=rgb(red,green,blue,0.3))
    axis.labels <- seq(plot.range[1],plot.range[2],length.out=7)
    if(axisX){  axis(1, axis.labels, sprintf("%.2f", axis.labels))         }
    if(axisY){  axis(2, axis.labels, sprintf("%.2f", axis.labels), las=2)  }
    if(add.titles){
      mtext("Simulated values",1, line=2.5, f=2, cex=1.2)
      mtext("Recovered values",2, line=2.75, f=2, cex=1.2)
    }
  }
}

#load("../../../../simulations/params_from_uniforms/sim_P20T20D1000_MetaRegEZBHDDM_genUnif.RData")
#simOutput <- output
#make_panel_type1(simOutput, parameter="bound", 
#                 add.titles = TRUE, plot.range=NA)



make_panel_type2 <- function(simOutput, parameter=NA, 
                             add.titles = FALSE, nBins=15, 
                             plot.range=NA, axisX=TRUE, axisY=TRUE){
  if(is.na(parameter)){ stop("Please specify a parameter")  }
  
  if(parameter=="drift"|parameter=="drift_mean"){          parameter <- "drift_mean"
  red <- 247/255 ;  green <- 167/255 ; blue <- 26/255
  }else{if(parameter=="bound"|parameter=="bound_mean"){    parameter <- "bound_mean"  
  red <- 188/255 ;  green <- 56/255 ; blue <- 156/255
  }else{if(parameter=="nondt"|parameter=="nondt_mean"){    parameter <- "nondt_mean"
  red <- 56/255 ;  green <- 188/255 ; blue <- 58/255
  }else{  red <- 0.2 ;  green <- 0.8 ; blue <- 0.6    }}}
  
    x <- simOutput$trueValues[,parameter]
    y <- simOutput$estimates[,parameter]
    n <- length(x)
    edges <- round(range(x, na.rm = TRUE),1)
    bins <- seq(edges[1],edges[2],length.out=nBins)
    
    if(sum(is.na(plot.range))>0){
      #plot.range <- range(c(x,y), na.rm = TRUE)
      plot.range <- edges
    }
    
    plot(10,10, xlim=plot.range, ylim=plot.range, ann=F, axes=F, col="white")
    if(add.titles){
      if(parameter=="betaweight"){
        mtext(expression(paste(beta, " coefficient")),3, line=1, f=2, cex=1.5)
      }else{
        if(parameter=="drift_mean"){
          mtext(expression(paste("Mean drift rate - ", mu[nu])),3, line=1, f=2, cex=1.5)
        }else{
          if(parameter=="bound_mean"){
            mtext(expression(paste("Mean boundary - ", mu[alpha])),3, line=1, f=2, cex=1.5)
          }else{
            if(parameter=="nondt_mean"){
              mtext(expression(paste("Mean nondecision time - ", mu[tau])),3, line=1, f=2, cex=1.5)
            }else{
              mtext(parameter,3, line=1, f=2, cex=1.5)
            }}}}
    }
    
    abline(v=mean(plot.range), col="gray80", lty=3)
    abline(h=mean(plot.range), col="gray80", lty=3)
    abline(0,1,col="gray70", lwd=1, lty=1)
    heights <- c()
    mids <- c()
    for(b in 2:length(bins)){
      X.inBin <- x[x<=bins[b]&x>=bins[b-1]]
      Y.inBin <- y[x<=bins[b]&x>=bins[b-1]]
      count <- length(X.inBin)
      whiskers <- quantile(Y.inBin,probs = c(0.025,0.5,0.975),na.rm=TRUE)
      prop  <- count/n
      heights <- rbind(heights,whiskers)
      mids <- append(mids,median(c(bins[b],bins[b-1])))
    }
    #points(mids, heights[,1], pch=16, cex=0.5)
    #points(mids, heights[,3], pch=16, cex=0.5)
    #points(mids, heights[,2], pch=16, cex=0.5)
    polygon(c(mids,rev(mids)), c(heights[,1],rev(heights[,3])),
            col=rgb(red,green,blue,0.09), border = NA)
    points(x,y, cex=0.6, pch=16, col=rgb(red/10,green/10,blue/10,0.3))
    axis.labels <- seq(plot.range[1],plot.range[2],length.out=7)
    lines(mids,heights[,1], lwd=1.5, col=rgb(red,green,blue,1))
    lines(mids,heights[,2], lwd=1)
    lines(mids,heights[,3], lwd=1.5, col=rgb(red,green,blue,1))
    if(axisX){  axis(1, axis.labels, sprintf("%.2f", axis.labels), cex.axis=0.78, las=2, line=-0.35,
                     col = rgb(0, 0, 0, alpha = 0), col.axis = "black") # Don't show line and ticks
                axis(1, axis.labels, rep("",length(axis.labels)), tcl = -0.3, col.axis = NA, 
                     col = rgb(0, 0, 0, alpha = 0), col.ticks = "black")                                }
    if(axisY){  axis(4, axis.labels, sprintf("%.2f", axis.labels), cex.axis=0.78, las=2, line=-0.45,
                     col = rgb(0, 0, 0, alpha = 0), col.axis = "black") # Don't show line and ticks
                axis(4, axis.labels, rep("",length(axis.labels)), tcl = -0.3, col.axis = NA, 
                     col = rgb(0, 0, 0, alpha = 0), col.ticks = "black")                                }
    if(add.titles){
      mtext("Simulated values",1, line=2.5, f=2, cex=1.2)
      mtext("Recovered values",2, line=2.75, f=2, cex=1.2)
    }
}

#load("../../../../simulations/params_from_uniforms/sim_P20T20D1000_MetaRegEZBHDDM_genUnif.RData")
#simOutput <- output
#make_panel_type1(simOutput, parameter="bound", 
#                 add.titles = TRUE, plot.range=NA)
#make_panel_type2(simOutput, parameter="bound", 
#                 add.titles = TRUE, nBins=11, plot.range=NA)
