#assign('B', get(load("../sim_P40Tc160D1000_FixedEffectNull.RData")))
#assign('B1', get(load("../sim_P40Tc160D1000_FixedEffectDiff1.RData")))
#assign('B2', get(load("../sim_P40Tc160D1000_FixedEffectDiff2.RData")))
#assign('B3', get(load("../sim_P40Tc160D1000_FixedEffectDiff3.RData")))
#assign('B4', get(load("../sim_P40Tc160D1000_FixedEffectDiff4.RData")))
#assign('B5', get(load("../sim_P40Tc160D1000_FixedEffectDiff5.RData")))

betaNoDiff <- B
betaDiff <- B5

pdf(file = "../../../figures/hypoTest_meanPosts.pdf", width = 6, height = 3.5)
par(pty="m", mai=c(0.7,0.25,0,0), oma= c(0,0,1,1.5), bg=NA)

  beta.0 <- betaNoDiff$estimates[,"betaweight"]
  beta.1 <- betaDiff$estimates[,"betaweight"]
  mpb0 <- mean(beta.0)
  mpb1 <- mean(beta.1)
  ci.b0 <- quantile(beta.0,probs = c(0.025,0.975))
  ci.b1 <- quantile(beta.1,probs = c(0.025,0.975))
  ci.b0.x <- density(beta.0)$x[which(density(beta.0)$x>ci.b0[1]&density(beta.0)$x<ci.b0[2])]
  ci.b0.y <- density(beta.0)$y[which(density(beta.0)$x>ci.b0[1]&density(beta.0)$x<ci.b0[2])]
  ci.b1.x <- density(beta.1)$x[which(density(beta.1)$x>ci.b1[1]&density(beta.1)$x<ci.b1[2])]
  ci.b1.y <- density(beta.1)$y[which(density(beta.1)$x>ci.b1[1]&density(beta.1)$x<ci.b1[2])]
  curve0 <-"#AEBBD0"
  line0 <- "#4B8CF4"
  curve1 <-"#E4C99A"
  line1 <- "#C47F07"
  maxY <- max(density(beta.0)$y,density(beta.1)$y)*1.15
  ci0 <- rgb(162/255,175/255,255/255,0.4)
  ci1 <- rgb(250/255,128/255,105/255,0.4)
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
  legend(0.48,7.7, expression(paste("True ", beta, " = ", 0.5)), cex=1.3, col="black", adj=0.1, bty="n")
  legend(-0.24,7.7, expression(paste("True ", beta, " = ", 0)), cex=1.3, col="black", adj=0.1, bty="n")
  legend(-0.1,1.2, paste("Average = ",round(mpb0,4)), cex=0.7, bg="#ABB3D8", adj=0.1, box.col = "#1637DC")
  legend(0.4,1.2, paste("Average = ",round(mpb1,4)), cex=0.7, bg="#E2A384", adj=0.1, box.col = "#FF5700")
  text(0.17,6, paste("95% mean posteriors"), cex=0.9, f=2, col=line0)
  text(0.17,5.5, paste("[", round(ci.b0[1],2),",",round(ci.b0[2],2),"]"), cex=0.8, col=line0, f=2)
  text(0.3,4.8, paste("95% mean posteriors"), cex=0.9, f=2, col=line1)
  text(0.3,4.3, paste("[", round(ci.b1[1],2),",",round(ci.b1[2],2),"]"), cex=0.8, col=line1, f=2)
  mtext(expression(paste("Mean posterior ", beta)),1,line=2.5, cex=1.1)

  dev.off()