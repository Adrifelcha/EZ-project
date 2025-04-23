#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function calculates the R-hat convergence diagnostic
#
# R-hat measures the convergence of MCMC chains by comparing the variance between
# chains to the variance within chains. Values close to 1.0 indicate good convergence.
# Generally, we consider values below 1.05 to be acceptable.
#
# Inputs:
# - posterior_chains: A matrix where columns store MCMC chains and rows store iterations
# - n.chains: Number of chains (only needed if posterior_chains has been reshaped into a vector)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
getRhat <- function(posterior_chains, n.chains=NA) {
        # Ensure the input is a matrix
        # If a vector is provided, reshape it into a matrix with n.chains columns
        if (is.vector(posterior_chains)) {
            if(is.na(n.chains)){
                  stop("Please specify the number of chains") 
            }else{
              # Calculate iterations per chain and reshape the vector into a matrix
              n.iter <- length(posterior_chains)/n.chains
              # We assume that chains had been stacked in the vector
              posterior_chains <- matrix(posterior_chains, ncol=n.chains, nrow = n.iter, byrow = FALSE)
            }
        }else{
            # Get dimensions of the posterior chains matrix
            n.iter <- nrow(posterior_chains)  # Number of iterations per chain
            n.chains <- ncol(posterior_chains)  # Number of chains
        }
        
        # Step 1: Compute the mean of each chain
        chainMean <- apply(posterior_chains, 2, mean)
        
        # Step 2: Compute the mean of the chain means
        overall_mean <- mean(chainMean)
        
        # Step 3: Compute between-chain variance (B)
        B <- n.iter * var(chainMean)
        
        # Step 4: Compute within-chain variances (W)
        chainVar <- apply(posterior_chains, 2, var)

        # Step 5: Compute the mean of the within-chain variances
        W <- mean(chainVar)
        
        # Step 6: Estimate the marginal posterior variance 
        Z <- ((n.iter - 1) / n.iter) * W + (1 / n.iter) * B
        
        # Step 7: Compute R-hat
        Rhat <- sqrt(Z / W)
  
  return(Rhat)
}

# Reference: https://medium.com/@nialloulton/understanding-the-r-hat-statistic-d83b3b5ca162
