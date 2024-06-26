assign('B', get(load("../../simulations/hypothesis_testing/sim_P40Tc160D1000_FixedEffectNull.RData")))

ROPE_lvls <- c(0.01,0.05,0.1,0.2)
crit_lvls <- c(1:5,10,15,10)

prior.mean <- B$settings$prior$betaweight_mean
prior.sd <- B$settings$prior$betaweight_sdev

for(ROPE in ROPE_lvls){
    prior_dens <- pnorm(ROPE,prior.mean,prior.sd) - pnorm(-ROPE,prior.mean,prior.sd)
    
    for(crit in crit_lvls){
    ROPE_postProp <- c()
        for(k in 1:dim(B$beta_chain)[3]){
          tmp <- mean(B$beta_chain[,,k]<ROPE&B$beta_chain[,,k]>(-ROPE))
          ROPE_postProp <- c(ROPE_postProp, tmp)
        }
    ratio <- ROPE_postProp/prior_dens
    print(mean(ratio>crit))
    }
}


