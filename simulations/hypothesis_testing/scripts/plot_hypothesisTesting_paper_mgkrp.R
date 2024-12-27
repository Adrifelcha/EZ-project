#assign('B', get(load("../sim_P40Tc160D1000_FixedEffectNull.RData")))
#assign('B1', get(load("../sim_P40Tc160D1000_FixedEffectDiff1.RData")))
#assign('B2', get(load("../sim_P40Tc160D1000_FixedEffectDiff2.RData")))
#assign('B3', get(load("../sim_P40Tc160D1000_FixedEffectDiff3.RData")))
#assign('B4', get(load("../sim_P40Tc160D1000_FixedEffectDiff4.RData")))
#assign('B5', get(load("../sim_P40Tc160D1000_FixedEffectDiff5.RData")))
#estimates <- list("B" = B, "B1" = B1, "B2" = B2, "B3" = B3, "B4" = B4, "B5" = B5)
estimates <- list("B" = B, "B1" = B1, "B2" = B2, "B3" = B3, "B4" = B4, "B5" = B5)
look.at <- c("B", "B2", "B4")
levelsM <- 1000
m <- seq(0,2,length.out=levelsM)
levelsM2 <- 10000
m2 <- seq(-10,10,length.out=levelsM2)
m2[1] <- -Inf
m2[levelsM2] <- Inf
p <- matrix(NA,ncol=length(look.at),nrow=levelsM)
p2 <- matrix(NA,ncol=length(look.at),nrow=levelsM2)
epsilon <- 0.1
prior_constant <- pnorm(epsilon)-pnorm(-epsilon)
BF <- c()
count <- 1
for(b in look.at){
    getB <- estimates[[b]]
    x <- getB$estimates[,"betaweight"]
    sd <- sqrt(getB$variance[,"betaweight"])
    check <- x/sd
    betas <- getB$beta_chain
    post_mass <- apply(betas, 3, function(x) mean(x > -epsilon & x < epsilon))
    this.BF <- log(prior_constant/post_mass)
    this.BF[post_mass==0] <- Inf
    BF <- cbind(BF,this.BF)
    is.infinite(BF)
    for(i in 1:levelsM){
        p[i,count] <- mean(abs(check)<=m[i], na.rm = TRUE)
    }
    for(i in 1:levelsM2){
      p2[i,count] <- mean(this.BF<=m2[i], na.rm = TRUE)
    }
    count <- 1+count
}

plot(m,p[,1], type="l", ylim=c(0,1), ann=F, axes=F)
for(j in 2:length(look.at)){
    lines(m,p[,j],col=j)
}


plot(p[,2],p[,1], type="l", ylim=c(0,1), xlim-c(0,1))
for(j in 3:length(look.at)){
  lines(p[,j],p[,1],col=j)
}



plot(m2,p2[,1], type="l", ylim=c(0,1), ann=F, axes=F)
for(j in 2:length(look.at)){
  lines(m2,p2[,j],col=j)
}


plot(p2[,2],p2[,1], type="l", ylim=c(0,1), xlim-c(0,1))
for(j in 3:length(look.at)){
  lines(p2[,j],p2[,1],col=j)
}


getB <- estimates[["B"]]
x <- getB$estimates[,"betaweight"]
plot(density(x), ylim=c(0,8), xlim=c(-0.2,0.7))
getB <- estimates[["B1"]]
x <- getB$estimates[,"betaweight"]
lines(density(x))
getB <- estimates[["B2"]]
x <- getB$estimates[,"betaweight"]
lines(density(x))
getB <- estimates[["B3"]]
x <- getB$estimates[,"betaweight"]
lines(density(x))
getB <- estimates[["B4"]]
x <- getB$estimates[,"betaweight"]
lines(density(x))
abline(v=c(-0.1,0.1), col="red")



#plot(1:10, (1:10)^2, type = "b", col = "blue", 
#     main = "Main Plot with Inset", xlab = "X-axis", ylab = "Y-axis")
#pdf(file = "../../../../../../../Dropbox/Aplicaciones/Overleaf/An EZ Bayesian hierarchical drift diffusion model for response time and accuracy/figures/roc.pdf", width = 4.5, height = 3) # Width and height of the plot in 
fileName <- "../../../../../../../Dropbox/Aplicaciones/Overleaf/An EZ Bayesian hierarchical drift diffusion model for response time and accuracy/figures/roc.eps"
postscript(fileName, horizontal = FALSE, onefile = FALSE, paper = "special", 
           width = 3.5, height = 2.15)
par(mai=c(0.4,0.4,0,0), oma= c(0,1.5,0,0), bg=NA)
plot(p2[,2],p2[,1], type="n", ylim=c(0,1), xlim=c(0,1), ann=F, axes=F, lwd=4)
for(j in 3:length(look.at)){
  lines(p2[,j],p2[,1],col="#3C9C3B", lwd=4)
}
lines(p2[,2],p2[,1],col="#5E18F1", lwd=4, lty=2)
x_lim <- c(0,1)
axis.cex <- 0.7
x_lab <- seq(0,1,length.out=7)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)
axis(1,x_lab, round(x_lab,1), cex.axis=axis.cex, lwd=0, line=-1.35)
axis(2,x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)
axis(2,x_lab, round(x_lab,1), cex.axis=axis.cex, lwd=0, line=-0.9, las=2)
mtext("true positive rate",1,line=0.8, f=2, cex=0.8)
mtext("false positive rate",2,line=1, f=2, cex=0.8)
lines(c(0.22,0.25),c(0.85,0.85),lwd=4, col="#5E18F1", lty=1)
lines(c(0.28,0.31),c(0.85,0.85),lwd=4, col="#5E18F1", lty=1)
text(0.41,0.84, expression(paste("True ", beta," = 0", sep="")), cex=0.6)
lines(c(0.61,0.67),c(0.85,0.85),lwd=4, col="#3C9C3B")
text(0.80,0.84, expression(paste("True ", beta," = 0.4", sep="")), cex=0.6)
# Save the current plotting parameters
old_par <- par(no.readonly = TRUE)

# Add a smaller plot in the bottom-right corner
par(fig = c(0.2, 0.9, 0.2, 0.84), new = TRUE)  # Define the inset's position
getB <- estimates[["B"]]
x <- getB$estimates[,"betaweight"]
x_lim <- c(-0.2,0.65)
max.Y <- c(max(density(x)$y,density(x)$y,density(x)$y))
hist(x, freq = FALSE, breaks = 50, col="#ADB1C6", border = "#666D95", 
     ann=F, axes = F, ylim = c(0, max.Y*1.4), xlim=x_lim)
lines(density(x), ylim=c(0,8), col="#192154", lwd=2)
getB <- estimates[["B2"]]
x <- getB$estimates[,"betaweight"]
hist(x, freq = FALSE, breaks = 50, col="#C8B6EE", border = "#7757BC",add=T)
lines(density(x), ylim=c(0,8), col="#5E18F1", lwd=2)
getB <- estimates[["B4"]]
x <- getB$estimates[,"betaweight"]
hist(x, freq = FALSE, breaks = 50, col="#E1F4BA", border = "#D2E689",add=T)
lines(density(x), ylim=c(0,8), col="#3C9C3B", lwd=2)
x_lab <- seq(x_lim[1],x_lim[2],length.out=6)
axis(1,x_lab, rep("", length(x_lab)), tck=-0.04, line=0)
axis(1,x_lab, round(x_lab,1), cex.axis=axis.cex*0.7, lwd=0, line=-1.1)
#polygon(c(-0.1,0.1,0.1,-0.1), c(0,0,max.Y,max.Y),
#        col=rgb(214/255,108/255,108/255,0.3), border = "red")
text(-0.1,max.Y*1.15,expression(paste("True ", beta," = 0", sep="")), cex=0.6, col="#192154")
text(0.2,max.Y*1.2,expression(paste("True ", beta," = 0.2", sep="")), cex=0.6, col="#5E18F1")
text(0.54,max.Y*1.1,expression(paste("True ", beta," = 0.4", sep="")), cex=0.6, col="#3C9C3B")
mtext(expression(paste("mean posterior estimate ", beta)),1, line=0.55, cex=0.7, f=2)
# Restore the original plotting parameters
par(old_par)
dev.off()