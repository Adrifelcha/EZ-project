###########################################################################
#
###########################################################################
load("../results/data_toJAGS.RData")
load("../results/samples.RData")
source("./loadSamples.R")

png(file = "../../../figures/mlr_allPosteriors.png", width = 7, height = 5, units="in",res=300) # Width and height of the plot in inches
#png(file = "../../figures/shapeEx_posteriors.png",width = 800, height=480, units="px") # Width and height of the plot in inches
layout(mat = matrix(c(1,1,2,2,3,3,
                      0,4,4,5,5,0), nrow = 2, byrow = TRUE))
layout.show(n = 5)
# Mai = Internal margins // Oma = Outer margin
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
dev.off()