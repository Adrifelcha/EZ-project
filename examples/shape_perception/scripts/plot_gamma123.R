###########################################################################
#
###########################################################################
load("../results/data_toJAGS.RData")
load("../results/samples.RData")
source("./loadSamples.R")

curve.gamma1 <- "#B2E5C7"
line.gamma1  <- "#419E67"
curve.gamma2 <- "#B6E5DB"
line.gamma2  <- "#2DAD92"
curve.gamma3 <- "#BED4E4"
line.gamma3  <- "#2975AC"

png(file = "../../../figures/mlr_gamma12and3.png", width = 7, height = 3, units="in",res=300) # Width and height of the plot in inches
# Mai = Internal margins // Oma = Outer margin
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
dev.off()