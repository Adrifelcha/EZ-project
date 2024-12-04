# A function taking as input a MATRIX, where the columns correspond to different MCMC chains and the rows are the iterations
getRhat <- function(posterior_chains, n.chains=NA) {
        # Ensure the input is a matrix
        if (is.vector(posterior_chains)) {
          if(is.na(n.chains)){
                stop("Please specify the number of chains") 
          }else{
            n.iter <- length(posterior_chains)/n.chains
             posterior_chains <- matrix(posterior_chains, ncol=n.chains, nrow = n.iter, byrow = FALSE)
          }
        }
        
        n.iter <- nrow(posterior_chains) # Number of iterations
        if(is.na(n.chains)){        n.chains <- ncol(posterior_chains)         }
        
        # Step 1: Compute the mean of each chain
        chainMean <- apply(posterior_chains, 2, mean)
        # Step 2: Compute the overall mean
        overall_mean <- mean(chainMean)
        
        # Step 3: Compute between-chain variance (B)
        B <- n * var(chainMean)
        
        # Step 4: Compute within-chain variances (W)
        chainVar <- apply(posterior_chains, 2, var)
        W <- mean(chainVar)
        
        # Step 5: Estimate the marginal posterior variance
        Z <- ((n - 1) / n) * W + (1 / n) * B
        
        # Step 6: Compute R-hat
        Rhat <- sqrt(Z / W)
  
  return(Rhat)
}

# Reference : https://medium.com/@nialloulton/understanding-the-r-hat-statistic-d83b3b5ca162
