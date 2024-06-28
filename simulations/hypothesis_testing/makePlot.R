assign('B', get(load("./sim_P40Tc160D1000_FixedEffectNull.RData")))
assign('B1', get(load("./sim_P40Tc160D1000_FixedEffectDiff1.RData")))
assign('B2', get(load("./sim_P40Tc160D1000_FixedEffectDiff2.RData")))
assign('B3', get(load("./sim_P40Tc160D1000_FixedEffectDiff3.RData")))
assign('B4', get(load("./sim_P40Tc160D1000_FixedEffectDiff4.RData")))
assign('B5', get(load("./sim_P40Tc160D1000_FixedEffectDiff5.RData")))

ROPE_lvls <- c(0.01,0.05,0.1,0.2)

prior.mean <- B$settings$prior$betaweight_mean
prior.sd <- B$settings$prior$betaweight_sdev

ROPE_dens <- array(NA, dim = c(dim(B$beta_chain)[3],6,length(ROPE_lvls)),
                   dimnames = list(1:dim(B$beta_chain)[3], 
                                   c("beta 0","beta 0.1","beta 0.2","beta 0.3","beta 0.4","beta 0.5"),
                                   paste("ROPE +/-", ROPE_lvls, sep = "")))
prior_dens <- matrix(NA,ncol=length(ROPE_lvls), dimnames = list("priorDensity", paste("+/-",ROPE_lvls,sep="")))
for(r in 1:length(ROPE_lvls)){
    ROPE <- ROPE_lvls[r]
    prior_dens[r] <- pnorm(ROPE,prior.mean,prior.sd) - pnorm(-ROPE,prior.mean,prior.sd)
        for(k in 1:dim(B$beta_chain)[3]){
            ROPE_dens[k,1,r] <- mean(B$beta_chain[,,k]<ROPE&B$beta_chain[,,k]>(-ROPE))
            ROPE_dens[k,2,r] <- mean(B1$beta_chain[,,k]<ROPE&B1$beta_chain[,,k]>(-ROPE))
            ROPE_dens[k,3,r] <- mean(B2$beta_chain[,,k]<ROPE&B2$beta_chain[,,k]>(-ROPE))
            ROPE_dens[k,4,r] <- mean(B3$beta_chain[,,k]<ROPE&B3$beta_chain[,,k]>(-ROPE))
            ROPE_dens[k,5,r] <- mean(B4$beta_chain[,,k]<ROPE&B4$beta_chain[,,k]>(-ROPE))
            ROPE_dens[k,6,r] <- mean(B5$beta_chain[,,k]<ROPE&B5$beta_chain[,,k]>(-ROPE))
        }
}


weightPrior <- array(NA, dim = c(dim(B$beta_chain)[3],6,length(ROPE_lvls)),
                     dimnames = list(1:dim(B$beta_chain)[3], 
                                c("beta 0","beta 0.1","beta 0.2","beta 0.3","beta 0.4","beta 0.5"),
                                   paste("ROPE +/-", ROPE_lvls, sep = "")))
for(i in 1:length(ROPE_lvls)){
  weightPrior[,,i] <- ROPE_dens[,,i]/prior_dens[i]
}


Dist.Null <- weightPrior[,1,]
Dist.Diff <- weightPrior[,2:6,]

crit_lvls <- c(2:5,10,15,10)
 
c <- crit_lvls[1]
FA <- apply(Dist.Null<c,2,mean)


