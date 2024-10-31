makeSimStudyPlot <- function(simStudyRData, param=NA, plotType=1, plot.range=NA, showParam = TRUE, showStudy = FALSE){
  assign('obj', get(load(simStudyRData)))
  lvls <- c(20,40,80,160,320)
  nL <- length(lvls)
  print_Yaxis <- rep(c(TRUE,rep(FALSE,nL-1)),nL)
  print_Xaxis <- c(rep(rep(FALSE,nL),nL-1),rep(TRUE,nL))
  P <- lvls
  Tr <- lvls
  if(sum(is.na(param))>0){   
        param <- c("drift_mean", "nondt_mean", "bound_mean")    
        if(grepl("Meta", simStudyRData)|grepl("Ttest", simStudyRData)){
           param <- c(param,"betaweight")
        }
  }
  
  for(par in param){
      par(pty="s", mfrow=c(5,5), mai=c(0,0,0.05,0.05), 
          oma=c(2.5,2.5,1.5,1.5))
      panel_no <- 1
      plot.range <- round(range(c(obj$true[[1]][,par,]
                                  ,obj$recovered[[1]][,par,]
                                  ), na.rm = TRUE),1)
      
      print(par)
      for(i in 1:5){
            thisP_x <- obj$true[[i]]
            thisP_y <- obj$recovered[[i]]
        for(k in 1:5){
                this.panel <- list("trueValues" = matrix(as.vector(thisP_x[,par,k]), ncol=1,
                                                        dimnames = list(list(),par)),
                                   "estimates" = matrix(as.vector(thisP_y[,par,k]), ncol=1,
                                                         dimnames = list(list(),par)))
            if(plotType==1){
                make_panel_type1(this.panel, parameter=par, 
                             add.titles = FALSE, plot.range=plot.range)
            }else{
                make_panel_type2(this.panel, parameter=par, 
                             add.titles = FALSE, nBins=15, 
                             plot.range=plot.range,
                             axisX = print_Xaxis[panel_no], 
                             axisY = print_Yaxis[panel_no])
            }
            box(lty=3)
            if(k==1){mtext(paste("P =",P[i]),2,line=2.5,f=2)}
            if(i==1){mtext(paste("T =",Tr[k]),line=0.5,f=2)}
            panel_no <- panel_no +1
            
            print(paste("k =", k, " and i =", i, sep=""))
            print(cbind(this.panel$estimates[1:2], this.panel$trueValues[1:2]))
        }
      }
      if(showParam){
              if(par=="drift_mean"){       label <-  expression(paste(mu[nu]))   }
              if(par=="nondt_mean"){       label <-  expression(paste(mu[tau]))   }
              if(par=="bound_mean"){       label <-  expression(paste(mu[alpha]))   }
              if(par=="betaweight"){  label <-  expression(paste(beta))   }
              mtext(label, 4, outer=T, las=2, line=-1.5, cex=2)
      }
  }
}

#check.par <- "bound_mean"
#simStudyRData <- "../../simulations/params_from_uniforms/simStudy1/results/simStudy_Meta_nondtx.RData"
#dev.new(width=8, height=12)
#makeSimStudyPlot(simStudyRData, param=NA, plotType=2)

#source("../../simulations/params_from_uniforms/simStudy1/figures/plot_simStudyOutput_outdated.R")
#makeSimStudyPlot(simStudyRData, param=check.par)