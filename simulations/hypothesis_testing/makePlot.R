############################
# LOAD SAMPLES
############################
assign('B', get(load("./sim_P40Tc160D1000_FixedEffectNull.RData")))
assign('B1', get(load("./sim_P40Tc160D1000_FixedEffectDiff1.RData")))
assign('B2', get(load("./sim_P40Tc160D1000_FixedEffectDiff2.RData")))
assign('B3', get(load("./sim_P40Tc160D1000_FixedEffectDiff3.RData")))
assign('B4', get(load("./sim_P40Tc160D1000_FixedEffectDiff4.RData")))
assign('B5', get(load("./sim_P40Tc160D1000_FixedEffectDiff5.RData")))

################################
# MAJOR SETTINGS AND VARIABLES
################################
ROPE_lvls <- c(0.01,0.05,0.1,0.2,0.3,0.5)
crit_lvls <- c(1.5,2:5,10)

prior.mean <- B$settings$prior$betaweight_mean
prior.sd <- B$settings$prior$betaweight_sdev

############################################################
# Prior and Posterior density accumulated per level of ROPE
############################################################
# Empty objects for storage 
ROPE_dens <- array(NA, dim = c(dim(B$beta_chain)[3],6,length(ROPE_lvls)),
                   dimnames = list(list(), paste("beta 0.", 0:5, sep=""), paste("+/-", ROPE_lvls, sep = "")))
prior_dens <- matrix(NA,ncol=length(ROPE_lvls), dimnames = list("priorDensity", paste("+/-",ROPE_lvls,sep="")))
# Compute 1) Prior and posterior density accumulated at each ROPE level
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
# Approach 1: median(Beta in ROPE)
############################################################
probs <- apply(ROPE_dens, c(2,3), median)
plot(probs[,1], ylim=c(0,1), type="b", col=1, ann=FALSE, axes=FALSE)
axis(2,seq(0,1,length.out=10),round(seq(0,1,length.out=10),1),las=2)
axis(1, 1:6, paste("Beta 0.",0:5, sep=""))
legend("topright", paste("ROPE +/-", ROPE_lvls, sep=""), col=1:6, pch=16, cex=0.7)
for(i in 2:6){
    lines(probs[,i], type="b", col=i)
}
mtext("median p(Beta in ROPE)",2, line=2.5, cex=1.3)
############################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############################################
# BF:  Posterior Density / Prior density
#############################################
weightPrior <- array(NA, dim = c(dim(B$beta_chain)[3],6,length(ROPE_lvls)),
                     dimnames = list(list(), paste("beta 0.", 0:5, sep=""), paste("+/-", ROPE_lvls, sep = "")))
for(i in 1:length(ROPE_lvls)){    weightPrior[,,i] <- ROPE_dens[,,i]/prior_dens[i]      }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
# Approach 2: The median posterior/prior ratio
############################################################
ratios <- apply(weightPrior, c(2,3), median)
plot(ratios[,1], ylim=c(0,13), type="b", col=1, ann=FALSE, axes=FALSE)
axis(2,seq(0,13,length.out=10),round(seq(0,13,length.out=10),0),las=2)
axis(1, 1:6, paste("Beta 0.",0:5, sep=""))
legend("topright", paste("ROPE +/-", ROPE_lvls, sep=""), col=1:6, pch=16, cex=0.7)
for(i in 2:6){
  lines(ratios[,i], type="b", col=i)
}
abline(h=1,lty=2)
mtext("median post(Beta in ROPE)/prior(Beta in ROPE)",2, line=2.5, cex=1.3)
############################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#############################################
#  Approach 3:  ROC-like approach
#############################################
Dist.Null <- weightPrior[,1,]      # <- Null is true
Dist.Diff <- weightPrior[,2:6,]    # <- Null is false

# Identify each BF ratio as Hits and False Alarms according to each criterion
Hits <- c()
FA <- array(NA, dim= c(5,length(ROPE_lvls),length(crit_lvls)),
            dimnames = list(list(), paste("+/-", ROPE_lvls, sep=""), paste("c =", crit_lvls)))
for(l in 1:length(crit_lvls)){
  c <- crit_lvls[l]
  Hits <- rbind(Hits, apply(Dist.Null>c,2,mean))  # p("Null"|Null)         HITS
  FA[,,l] <- apply(Dist.Diff>c,c(2,3),mean)    # p("Null"|Not-Null)     F.ALARMS
}
rownames(Hits) <- paste("c =", crit_lvls)

#############
# Plot 1: 
###############
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
for(i in 1:5){
  plot(FA[1,,i],Hits[i,], type="b", xlim=c(0,1), ylim=c(0,1), col=i)
  lines(FA[2,,i],Hits[i,], type="b", col=i)
  lines(FA[3,,i],Hits[i,], type="b", col=i)
  lines(FA[4,,i],Hits[i,], type="b", col=i)
  lines(FA[5,,i],Hits[i,], type="b", col=i)  
}

par(mfrow=c(1,1))
plot(FA[1,,i],Hits[i,], type="b", xlim=c(0,1), ylim=c(0,1), col=i)
lines(FA[2,,i],Hits[i,], type="b", col=i)
lines(FA[3,,i],Hits[i,], type="b", col=i)
lines(FA[4,,i],Hits[i,], type="b", col=i)
lines(FA[5,,i],Hits[i,], type="b", col=i)  










plot(FA[1,1,],Hits[,1],type="b", xlim=c(0,1), ylim=c(0,1))
lines(FA[2,1,],Hits[,1],type="b")
lines(FA[3,1,],Hits[,1],type="b")
lines(FA[4,1,],Hits[,1],type="b")
lines(FA[5,1,],Hits[,1],type="b")