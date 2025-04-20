###########################################################################################
# Load results and necessary packages
############################################################################################
# Load packages
library(here)
# Load custom functions
source(here("src", "plot_VerticalHist.R"))
# Load results
results_at <- here("output", "RData-results", "demo_shape_results.RData")
if(file.exists(results_at)){
  load(results_at)
} else {
  source(here("demos", "applications", "shape_perception", "src", "run_hypoTesting-example.R"))
}


###########################################################################################
# Prepare plotting variables and auxiliary functions
############################################################################################
# Custom function to select colors
myCol <- function(r,g,b,sub,alpha=1){
       rgb(r[sub]/255,g[sub]/255,b[sub]/255,alpha)
}

### Plotting functions
curve.gamma1 <- "#B2E5C7"
line.gamma1  <- "#419E67"
curve.gamma2 <- "#B6E5DB"
line.gamma2  <- "#2DAD92"
curve.gamma3 <- "#BED4E4"
line.gamma3  <- "#2975AC"
curve.gamma4 <- "#E4D4BE"
line.gamma4  <- "#AC7529"
curve.mu <- "#E4B6BE"
line.mu  <- "#AC2975"
base.recangle.bg <- "#F2EFE9"    
base.recangle.border <- "#C8B8A9" 


binWidth = 0.1
binStarts <- c(0.9,1.1,1.9,2.1)
binMids <- binStarts + binWidth / 2

axisCol <- "gray30"
xlim <- c(0.5,2.5)
ylim <- c(-1.2,2.1)
xlabs <- c("Change in Convexity", "Change in Concavity")
fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
CIcolor <- rep("#CAC7DA",4)

#### Process results
driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")
means <- apply(driftPred, 2, mean)
CI <- apply(driftPred, 2, quantile, prob=c(0.025,0.975))
DOYrange <- range(driftPred)
histList <- apply(driftPred, 2, function(x, hCol) hist(x, plot = FALSE))

shadeCI <- list()
for(i in 1:ncol(CI)){
  keep <- (driftPred[,i] > CI[1,i]) & (driftPred[,i] < CI[2,i])
  nBreaks <- sum(histList[[i]]$breaks >= CI[1,i] & histList[[i]]$breaks <= CI[2,i])
  partial <- hist(driftPred[keep,i], breaks = nBreaks, plot = FALSE)
  shadeCI <- append(shadeCI, list(partial))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 1: Main results (Gamma 1, 2, 3, and drift predictions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_main_results <- function(){
     par(mar=c(3.5,1.5,1.5,1), bg=NA)
     layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))

     max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
     hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
          ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,1]), lwd=4, col=line.gamma1)
     mtext("Change quality", cex=0.9, f=2, side=3, line=0.1)
     mtext("Posterior Density",side=2,line=0, cex=0.8)
     mtext(expression(paste(gamma[1])),side=1,line=1.9, cex=1)
     abline(v=0,lty=2,col="gray50")
     axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-0.4, cex.axis=0.8)
     legend(-0.2,1.6, c("B = 1  Quantitative", "B = 0  Qualitative"), bty = "n", bg = "white", cex=0.8)

     hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
          ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,2]), lwd=4, col=line.gamma2)
     mtext("Change type", cex=0.9, f=2, side=3, line=0.1)
     mtext(expression(paste(gamma[2])),side=1,line=1.9, cex=1)
     abline(v=0,lty=2,col="gray50")
     legend(-1.8,1.6, c("C = 1  Concavity", "C = 0  Convexity"), bty = "n", bg = "white", cex=0.8)
     axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-0.4, cex.axis=0.8)

     hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
          ann=F, axes = F, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,3]), lwd=4, col=line.gamma3)
     mtext("Change quality x type", cex=0.9, f=2, side=3, line=0.1)
     mtext(expression(paste(gamma[3])),side=1,line=1.9, cex=1)
     abline(v=0,lty=2,col="gray50")
     par(mar=c(2,14,0.5,14))
     axis(1,seq(-1.5,1.5,length.out=10),round(seq(-1.5,1.5,length.out=10),1), line=-2, cex.axis=0.8)
     
     plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
          ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
     axis(1, 1:2, xlabs, cex.axis = 1.2, col = NA, line=-0.5, f=1)
     #mtext(side = 1, outer = F, line = 2.5, "Change type", cex = 1.5, f=2)
     y.seq = format(round(seq(ylim[1],ylim[2],length.out=9),digits = 1), nsmall = 1)
     axis(2, cex.axis = 0.95, las = 1, line = -.7, col = "white", tck = 0,
          at = y.seq, labels = y.seq, las=2)
     mtext(side = 2, outer = F, line = 2, expression(paste(nu^pred)), cex = 1.1)
     box(bty = "L", col = axisCol)

     biggestDensity <- max(unlist(lapply(histList, function(h){max(h[[4]])})))
     xscale <- binWidth * .9 / biggestDensity

     ## Plot the histograms
     for (i in 1:4) {
     X <- binStarts[i]
     VerticalHist(x = X, xscale = xscale, xwidth = binWidth, 
                    hist= histList[[i]], fillCol = "black")
     VerticalHist(x = X, xscale = xscale*0.8, xwidth = binWidth, 
                    hist= shadeCI[[i]], fillCol = fillCol[i])
     points(X, means[i], pch=18, cex=1.2)
     lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=2)
     }
     lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
     lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
     legend(0.5,2.05, c("Qualitative change", "Quantitative change"), col=c("#42B6EC", "#0D41D5"), pch=15, bty = "n", cex=1.2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 2: All posterior distributions (Mu, and Gamma 1 to 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_all_posteriors <- function(){
     layout(mat = matrix(c(1,1,2,2,3,3, 0,4,4,5,5,0), nrow = 2, byrow = TRUE))     
     par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,1,1.5), bg=NA)

     max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
     hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,1]), lwd=4, col=line.gamma1)
     mtext("Quantitative change", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 1",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.55))
     lines(density(gamma[,2]), lwd=4, col=line.gamma2)
     mtext("Change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 2",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,3]), lwd=4, col=line.gamma3)
     mtext("Quantitative change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Interaction effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 3",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     max.Y <- c(max(density(gamma[,4])$y,density(mu)$y))+0.1
     hist(gamma[,4], freq = FALSE, breaks = 50, col=curve.gamma4, border = curve.gamma4, 
          ann=F, axes = T, ylim=c(0, max.Y))
     lines(density(gamma[,4]), lwd=4, col=line.gamma4)
     mtext("Change NOT occuring", cex=0.75, f=2, side=3, line=0.5)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=-0.6, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 4",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(mu, freq = FALSE, breaks = 50, col=curve.mu, border = curve.mu, ann=F, 
          axes = T, ylim=c(0, max.Y))
     lines(density(mu), lwd=4, col=line.mu)
     mtext("Qualitative change in Convexity", cex=0.75, f=2, side=3, line=0.5)
     mtext("(Intercept; Baseline drift)", cex=0.75, f=2, side=3, line=-0.6, col="gray30")
     mtext("Mu",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 3: Main posterior distributions (Gamma 1, 2, 3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_mainGamma_posteriors <- function(){
     par(pty="m", mfrow=c(1,3), mai=c(0.45,0.5,0.25,0), oma= c(0,0,1,1.5), bg=NA)

     max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))
     hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,1]), lwd=4, col=line.gamma1)
     mtext("Quantitative change", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Density",side=2,line=2.15, cex=0.8)
     mtext("Gamma 1",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.55))
     lines(density(gamma[,2]), lwd=4, col=line.gamma2)
     mtext("Change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Main effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 2",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")

     hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
          ann=F, axes = T, ylim = c(0, max.Y), xlim=c(-1.5,1.5))
     lines(density(gamma[,3]), lwd=4, col=line.gamma3)
     mtext("Quantitative change in Concavity", cex=0.75, f=2, side=3, line=1.2)
     mtext("(Interaction effect)", cex=0.75, f=2, side=3, line=0.1, col="gray30")
     mtext("Gamma 3",side=1,line=2, cex=0.75)
     abline(v=0,lty=2,col="gray50")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 4: Drift rate predictions per condition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_drift_prediction <- function(){
     plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim, ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
     axis(1, 1:2, xlabs, cex.axis = 1.2, col = NA, line=-0.5, f=1)
     mtext(side = 1, outer = F, line = 2.5, "Change type", cex = 1.5, f=2)
     y.seq = format(round(seq(ylim[1],ylim[2],length.out=9),digits = 1), nsmall = 1)
     axis(2, cex.axis = 0.95, las = 1, line = -.7, col = "white", tck = 0,
          at = y.seq, labels = y.seq, las=2)
     mtext(side = 2, outer = F, line = 2, expression(paste(nu^pred)), cex = 1.2)
     box(bty = "L", col = axisCol)

     biggestDensity <- max(unlist(lapply(histList, function(h){max(h[[4]])})))
     xscale <- binWidth * .9 / biggestDensity

     ## Plot the histograms
     for (i in 1:4) {
     X <- binStarts[i]
     VerticalHist(x = X, xscale = xscale, 
                    xwidth = binWidth, 
                    hist= histList[[i]], 
                    fillCol = fillCol[i])
     points(X, means[i], pch=18, cex=1.2)
     lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=2)
     }
     lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
     lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
     legend(0.5,1.9, c("Qualitative change", "Quantitative change"), col=c("#42B6EC", "#0D41D5"), pch=15, bty = "n", cex=1.2)
     text(0.9,1.9,"Change quality", cex=1.5, f=2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 5 to 7: Posterior predictions of accuracy rate, mean RT, and RT variance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accuracy rate
plot_ppAcc <- function(){
     iter <- nrow(pp_accRate)
     jitter.x <- runif(iter,-0.12,0.12)

     layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
     par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,5,1.5))
     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(0,1), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),paste("i =",1:9), line=-0.65)
          axis(2,seq(0,1,0.1), seq(0,1,0.1),las=1)
          mtext("Participant",1,line=1.7,cex=0.8)
          for(i in 1:9){
            this.Y <- pp_accRate[,seq(k,ncol(pp_accRate),5)[i]]
            CI <- quantile(this.Y, probs = c(0.025,0.975))
            polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col=base.recangle.bg, border=base.recangle.border)
            points(i+jitter.x,this.Y, col=myCol(r[,k],g[,k],b[,k],i,0.05), pch=16, cex=0.8)
            points(i, ezdata$acc_rate[which(ezdata$cond==k)][i], pch=8)
          }
          mtext(paste("Condition", k), f=2, line=1.5, cex=0.9)
          if(k==1){mtext("Qualitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==2){mtext("Quantitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==3){mtext("Qualitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==4){mtext("Quantitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==5){mtext("No change at all", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==1|k==4){ mtext("Accuracy rate", 2, line=2.5, cex=0.8) }
          mtext("Accuracy rate per condition per participant",3,outer=TRUE, f=2, line=2, cex=1.8)
     }
}
# Mean RT
plot_ppMeanRT <- function(){
     iter <- nrow(pp_varRT)
     jitter.x <- runif(iter,-0.12,0.12)
     minY <- min(pp_meanRT)
     maxY <- max(pp_meanRT)
     layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
     par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,5,1.5))
     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),paste("i =",1:9), line=-0.65)
          axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1)
          mtext("Participant",1,line=1.7,cex=0.8)
          for(i in 1:9){
            this.Y <- pp_meanRT[,seq(k,ncol(pp_meanRT),5)[i]]
            CI <- quantile(this.Y, probs = c(0.025,0.975))
            polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col=base.recangle.bg, border=base.recangle.border)
            points(i+jitter.x,this.Y, col=myCol(r[,k],g[,k],b[,k],i,0.05), pch=16, cex=0.8)
            points(i, ezdata$meanRT[which(ezdata$cond==k)][i], pch=8)
          }          
          mtext(paste("Condition", k), f=2, line=1.5, cex=0.9)
          if(k==1){mtext("Qualitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==2){mtext("Quantitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==3){mtext("Qualitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==4){mtext("Quantitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==5){mtext("No change at all", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==1|k==4){ mtext("Mean RT (secs)", 2, line=2.5, cex=0.8) }
          mtext("Mean RT per condition per participant",3,outer=TRUE, f=2, line=2, cex=1.8)
     }
}
# RT Variance
plot_ppvarRT <- function(){
     iter <- nrow(pp_meanRT)
     jitter.x <- runif(iter,-0.12,0.12)
     ppl_varRT <- log(pp_varRT)
     minY <- min(ppl_varRT)
     maxY <- max(ppl_varRT)
     layout(mat = matrix(c(1,1,2,2,3,3, 0,4,4,5,5,0), nrow = 2, byrow = TRUE))
     par(pty="m", mai=c(0.45,0.5,0.25,0), oma= c(0,0,5,1.5))
     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),paste("i =",1:9), line=-0.65)
          axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1)
          mtext("Participant",1,line=1.7,cex=0.8)
          for(i in 1:9){
            this.Y <- ppl_varRT[,seq(k,ncol(ppl_varRT),5)[i]]
            CI <- quantile(this.Y, probs = c(0.025,0.975))
            polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col=base.recangle.bg, border=base.recangle.border)
            points(i+jitter.x,this.Y, col=myCol(r[,k],g[,k],b[,k],i,0.05), pch=16, cex=0.8)
            points(i, log(ezdata$varRT[which(ezdata$cond==k)][i]), pch=8)
          }
          mtext(paste("Condition", k), f=2, line=1.5, cex=0.9)
          if(k==1){mtext("Qualitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==2){mtext("Quantitative change in Convexity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==3){mtext("Qualitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==4){mtext("Quantitative change in Concavity", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==5){mtext("No change at all", f=2, line=.5, cex=0.7, col="gray35")}
          if(k==1|k==4){ mtext("RT variance (log-scale)", 2, line=2.5, cex=0.8) }
          mtext("RT variance per condition per participant",3,outer=TRUE, f=2, line=2, cex=1.8)
     }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All posterior predictions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_all_postpred_checks <- function(){
     iter <- nrow(pp_meanRT)
     jitter.x <- runif(iter,-0.12,0.12)
     layout(mat = matrix(1:15, ncol=3, byrow=FALSE))     
     par(pty="m", mai=c(0.1,0.5,0,0), oma= c(2.4,3.4,2.5,0.4), bg=NA)

     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(0,1), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
          axis(2,seq(0,1,0.1), seq(0,1,0.1),las=1, cex.axis=0.7)
          for(i in 1:9){
               this.Y <- pp_accRate[,seq(k,ncol(pp_accRate),5)[i]]
               CI <- quantile(this.Y, probs = c(0.025,0.975))
               polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
               points(i+jitter.x,this.Y, col=myCol(r[,1],g[,1],b[,1],i,0.05), pch=16, cex=0.8)
               points(i, ezdata$acc_rate[which(ezdata$cond==k)][i], pch=8)
          }
          mtext(paste("Condition", k), 2, f=2, line=6, cex=1)
          mtext("Accuracy rate", 2, line=2.1, cex=0.6) 
          if(k==1){
          mtext("Accuracy rate", f=2, line=.5, cex=1)
          mtext("Qualitative change", 2, f=2, line=5, cex=0.75, col="gray50")
          mtext("in Convexity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
          if(k==2){
          mtext("Quantitative change", 2, f=2, line=5, cex=0.75, col="gray50")
          mtext("in Convexity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
          if(k==3){
          mtext("Qualitative change", 2, f=2, line=5, cex=0.75, col="gray50")
          mtext("in Concavity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
          if(k==4){
          mtext("Quantitative change", 2, f=2, line=5, cex=0.75, col="gray50")
          mtext("in Concavity", 2, f=2, line=4.1, cex=0.75, col="gray50")}
          if(k==5){mtext("No change at all", 2, f=2, line=5, cex=0.75, col="gray50")
          mtext("Participant",1,line=1.7,cex=0.8)
          axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, tick=FALSE, cex.axis=0.8)}
     }

     minY <- min(pp_meanRT)
     maxY <- max(pp_meanRT)
     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
          axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1, cex.axis=0.7)
          for(i in 1:9){
               this.Y <- pp_meanRT[,seq(k,ncol(pp_meanRT),5)[i]]
               CI <- quantile(this.Y, probs = c(0.025,0.975))
               polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
               points(i+jitter.x,this.Y, col=myCol(r[,2],g[,2],b[,2],i,0.05), pch=16, cex=0.8)
               points(i, ezdata$meanRT[which(ezdata$cond==k)][i], pch=8)
          }
          mtext("Mean RT (secs)", 2, line=2.1, cex=0.6) 
          if(k==1){mtext("Mean RT (secs)", f=2, line=.5, cex=1)}
          if(k==5){mtext("Participant",1,line=1.7,cex=0.8)
                    axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, tick=FALSE, cex.axis=0.8)}
     }

     ppl_varRT <- log(pp_varRT)
     minY <- min(ppl_varRT)
     maxY <- max(ppl_varRT)
     for(k in 1:5){
          plot(4.5, 10, col="white", ylim=c(minY,maxY), xlim=c(0.5,9.5),ann=F,axes=F)
          axis(1,seq(1,9,length.out=9),rep("",9), line=-0.65)
          axis(2,seq(minY,maxY,length.out=10), round(seq(minY,maxY,length.out=10),1),las=1, cex.axis=0.7)
          for(i in 1:9){
               this.Y <- ppl_varRT[,seq(k,ncol(ppl_varRT),5)[i]]
               CI <- quantile(this.Y, probs = c(0.025,0.975))
               polygon(x=c(i-0.4,i+0.4,i+0.4,i-0.4),y=c(CI[1],CI[1],CI[2],CI[2]), col="gray85", border="gray70")
               points(i+jitter.x,this.Y, col=myCol(r[,3],g[,3],b[,3],i,0.05), pch=16, cex=0.8)
               points(i, log(ezdata$varRT[which(ezdata$cond==k)][i]), pch=8)
          }
          mtext("RT variance (log-scale)", 2, line=2.1, cex=0.6)
          if(k==1){      mtext("RT variance (log-scale)", f=2, line=.5, cex=1)                 }
          if(k==5){      mtext("Participant",1,line=1.7,cex=0.8)
                         axis(1,seq(1,9,length.out=9),paste("p", 1:9, sep=""), line=-1, 
                         tick=FALSE, cex.axis=0.8)                                             }     
     }
}


###########################################################################################
# Store plots to output folder
###########################################################################################

# Main results
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_mainResults.png"), width = 10, height = 6, units="in", res=300)
plot_main_results()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_mainResults.pdf"), width = 10, height = 6)
plot_main_results()
dev.off()

# All posterior distributions
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_allPosteriors.png"), width = 10, height = 6, units="in", res=300)
plot_all_posteriors()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_allPosteriors.pdf"), width = 10, height = 6)
plot_all_posteriors()
dev.off()

# Main posterior distributions (Gamma 1, 2, 3)
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_mainGammaPosteriors.png"), width = 7, height = 3, units="in", res=300)
plot_mainGamma_posteriors()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_mainGammaPosteriors.pdf"), width = 7, height = 3)
plot_mainGamma_posteriors()
dev.off()

# Drift rate predictions per condition
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_driftPredictions.png"), width = 7, height = 3, units="in", res=300)
plot_drift_prediction()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_driftPredictions.pdf"), width = 7, height = 3)
plot_drift_prediction()
dev.off()

# Posterior predictions - Accuracy rate
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppAcc.png"), width = 7, height = 5, units="in", res=300)
plot_ppAcc()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppAcc.pdf"), width = 7, height = 5)
plot_ppAcc()
dev.off()
# Posterior predictions - Mean RT
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppMeanRT.png"), width = 7, height = 5, units="in", res=300)
plot_ppMeanRT()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppMeanRT.pdf"), width = 7, height = 5)
plot_ppMeanRT()
dev.off()
# Posterior predictions - RT variance
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppvarRT.png"), width = 7, height = 5, units="in", res=300)
plot_ppvarRT()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_ppvarRT.pdf"), width = 7, height = 5)
plot_ppvarRT()
dev.off()

# All posterior predictions
png(file = here("output", "figures", "results-presentation", "slides_shapeExample_allPostpredChecks.png"), width = 10, height = 10, units="in", res=300)
plot_all_postpred_checks()
dev.off()
pdf(file = here("output", "figures", "results-presentation", "slides_shapeExample_allPostpredChecks.pdf"), width = 10, height = 10)
plot_all_postpred_checks()
dev.off()

