########################################################################################
# Part 1: Load simulation results from RData files  
########################################################################################
library(here)
if(file.exists(here("output", "RData-results", "simHypTesting_P40T80_B.RData"))){
          assign('B', get(load(here("output", "RData-results", "simHypTesting_P40T80_B.RData"))))
          assign('B1', get(load(here("output", "RData-results", "simHypTesting_P40T80_B1.RData"))))
          assign('B2', get(load(here("output", "RData-results", "simHypTesting_P40T80_B2.RData"))))
}else{
     cat("Error: The RData files do not exist. Please run the simulation first.\n")
     cat("Run the simulation using the following command:\n")
     cat("Rscript ../runParallelSeeds.R\n")     
}

betaNoDiff <- B
betaDiff <- B2

plot_betaDistributions <- function(){
     par(pty="m", mai=c(0.7,0.25,0.1,0), oma= c(0,0,0,0.1), bg=NA)

     trueBeta0 <- unique(betaNoDiff$true[,"betaweight"])
     beta.0 <- betaNoDiff$estimates[,"betaweight"]
     mpb0 <- mean(beta.0)
     ci.b0 <- quantile(beta.0,probs = c(0.025,0.975))
     ci.b0.x <- density(beta.0)$x[which(density(beta.0)$x>ci.b0[1]&density(beta.0)$x<ci.b0[2])]
     ci.b0.y <- density(beta.0)$y[which(density(beta.0)$x>ci.b0[1]&density(beta.0)$x<ci.b0[2])]
     ci1 <- rgb(250/255,128/255,105/255,0.4)
     curve0 <-"#AEBBD0"
     line0 <- "#4B8CF4"     
     
     trueBeta1 <- unique(betaDiff$true[,"betaweight"])
     beta.1 <- betaDiff$estimates[,"betaweight"]
     mpb1 <- mean(beta.1)     
     ci.b1 <- quantile(beta.1,probs = c(0.025,0.975))
     ci.b1.x <- density(beta.1)$x[which(density(beta.1)$x>ci.b1[1]&density(beta.1)$x<ci.b1[2])]
     ci.b1.y <- density(beta.1)$y[which(density(beta.1)$x>ci.b1[1]&density(beta.1)$x<ci.b1[2])]
     ci0 <- rgb(162/255,175/255,255/255,0.4)
     curve1 <-"#E4C99A"
     line1 <- "#C47F07"

     maxY <- max(density(beta.0)$y,density(beta.1)$y)*1.15
     hist(beta.0, freq = FALSE, breaks = 40, col=curve0, border = "black", ylim = c(0,maxY),
          ann=F, axes = F, xlim=c(-0.2,0.7))
     hist(beta.1, freq = FALSE, breaks = 40, col=curve1, border = "black", add=TRUE, ylim = c(0,maxY))
     axis(1,seq(-0.2,0.7,0.05),seq(-0.2,0.7,0.05))
     lines(density(beta.0), lwd=4, col="black")
     lines(density(beta.1), lwd=4, col="black")
     polygon(x=c(ci.b0.x,rev(ci.b0.x)),c(rep(0,length(ci.b0.y)),rev(ci.b0.y)), col=ci0, border=line0,lwd=3)
     polygon(x=c(ci.b1.x,rev(ci.b1.x)),c(rep(0,length(ci.b1.y)),rev(ci.b1.y)), col=ci1, border=line1,lwd=3)
     abline(v=c(0,0.5), lwd=3, lty=3)
     points(mpb0,0,pch=15,col="#1637DC", cex=1.5)
     points(mpb1,0,pch=15,col="#FF5700", cex=1.5)
     legend(0.48,6, bquote(paste("True ", beta, " = ", .(trueBeta1))), cex=1.3, col="black", adj=0.1, bty="n")
     legend(-0.28,6, bquote(paste("True ", beta, " = ", .(trueBeta0))), cex=1.3, col="black", adj=0.1, bty="n")
     legend(-0.12,1.2, paste("Average = ",round(mpb0,4)), cex=0.8, bg="#7db8ff", adj=0.1, box.col = "#1637DC")
     legend(0.28,1.2, paste("Average = ",round(mpb1,4)), cex=0.8, bg="#fdde68", adj=0.1, box.col = "#FF5700")
     text(0.15,6, paste("95% mean posteriors"), cex=0.9, f=2, col=line0)
     text(0.14,5.5, paste("[", round(ci.b0[1],2),",",round(ci.b0[2],2),"]"), cex=0.8, col=line0, f=2)
     text(0.21,4.5, paste("95% mean posteriors"), cex=0.9, f=2, col=line1)
     text(0.26,4, paste("[", round(ci.b1[1],2),",",round(ci.b1[2],2),"]"), cex=0.8, col=line1, f=2)
     mtext(expression(paste("Mean posterior ", beta)),1,line=2.5, cex=1.1)
}

pdf(here("output", "figures", "paper_hypothesisAppendix_Betas.pdf"), width = 6, height = 3.5)
plot_betaDistributions()
dev.off()

png(here("output", "figures", "paper_hypothesisAppendix_Betas.png"), width = 6, height = 3.5, units = "in", res = 300)
plot_betaDistributions()
dev.off()

postscript(here("output", "figures", "paper_hypothesisAppendix_Betas.eps"), horizontal = FALSE, onefile = FALSE, paper = "special", width = 6, height = 3.5)
plot_betaDistributions()
dev.off()