#assign('B', get(load("../sim_P40Tc160D1000_FixedEffectNull.RData")))
#assign('B1', get(load("../sim_P40Tc160D1000_FixedEffectDiff1.RData")))
#assign('B2', get(load("../sim_P40Tc160D1000_FixedEffectDiff2.RData")))
#assign('B3', get(load("../sim_P40Tc160D1000_FixedEffectDiff3.RData")))
#assign('B4', get(load("../sim_P40Tc160D1000_FixedEffectDiff4.RData")))
#assign('B5', get(load("../sim_P40Tc160D1000_FixedEffectDiff5.RData")))
estimates <- list("B" = B, "B1" = B1, "B2" = B2, "B3" = B3, "B4" = B4, "B5" = B5)

levelsM <- 1000
m <- seq(0,2,length.out=levelsM)
p <- matrix(NA,ncol=length(estimates),nrow=levelsM)
for(b in 1:length(estimates)){
    getB <- estimates[[b]]
    x <- getB$estimates[,"betaweight"]
    sd <- sqrt(getB$variance[,"betaweight"])
    check <- x/sd
    for(i in 1:levelsM){
        p[i,b] <- mean(abs(check)<m[i], na.rm = TRUE)
    }
}

plot(m,p[,1], type="l", ylim=c(0,1), ann=F, axes=F)
for(j in 2:length(estimates)){
    lines(m,p[,j],col=j)
}


plot(p[,2],p[,1], type="l", ylim=c(0,1))
for(j in 3:length(estimates)){
  lines(p[,j],p[,1],col=j)
}


b


