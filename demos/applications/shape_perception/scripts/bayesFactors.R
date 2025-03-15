load("../results/samples.RData")

gamma <- samples$BUGSoutput$sims.list$gamma

epsilon <- 0.1
prior_constant <- pnorm(epsilon) - pnorm(-epsilon)
for(i in 1:3){
    g <- gamma[,i]
    post_mass <- mean(g > -epsilon & g < epsilon)
    this.BF <- prior_constant/post_mass
    this.BF[post_mass==0] <- 0
    cat("The B.F. for gamma", i, "in favor of an effect is", this.BF, "\n")
}