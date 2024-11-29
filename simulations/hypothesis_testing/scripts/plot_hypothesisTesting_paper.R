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
m2 <- seq(1,3,length.out=levelsM)
p <- matrix(NA,ncol=length(estimates),nrow=levelsM)
p2 <- p
BF_crit <- 0.2
prior_constant <- pnorm(BF_crit)-pnorm(-BF_crit)
BF <- c()
count <- 1
for(b in look.at){
    getB <- estimates[[b]]
    x <- getB$estimates[,"betaweight"]
    sd <- sqrt(getB$variance[,"betaweight"])
    check <- x/sd
    betas <- getB$beta_chain
    post_mass <- apply(betas, 3, function(x) mean(x > -BF_crit & x < BF_crit))
    this.BF <- prior_constant/post_mass
    BF <- cbind(BF,this.BF)
    for(i in 1:levelsM){
        p[i,count] <- mean(abs(check)<m[i], na.rm = TRUE)
        p2[i,count] <- mean(abs(this.BF)<m2[i], na.rm = TRUE)
    }
    count <- 1+count
}

plot(m,p[,1], type="l", ylim=c(0,1), ann=F, axes=F)
for(j in 2:length(estimates)){
    lines(m,p[,j],col=j)
}


plot(p[,2],p[,1], type="l", ylim=c(0,1))
for(j in 3:length(estimates)){
  lines(p[,j],p[,1],col=j)
}



plot(m2,p2[,1], type="l", ylim=c(0,1), ann=F, axes=F)
for(j in 2:length(estimates)){
  lines(m2,p2[,j],col=j)
}


plot(p2[,2],p2[,1], type="l", ylim=c(0,1))
for(j in 3:length(estimates)){
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



plot(1:10, (1:10)^2, type = "b", col = "blue", 
     main = "Main Plot with Inset", xlab = "X-axis", ylab = "Y-axis")

# Save the current plotting parameters
old_par <- par(no.readonly = TRUE)

# Add a smaller plot in the bottom-right corner
par(fig = c(0.6, 0.95, 0.05, 0.4), new = TRUE)  # Define the inset's position
plot(1:5, (1:5)^3, type = "l", col = "red", 
     xlab = "Inset X", ylab = "Inset Y", main = "Inset Plot")

# Restore the original plotting parameters
par(old_par)