#################################################################################
# HYPOTHESIS TESTING VISUALIZATION
#                                                    Adriana F. Chávez De la Peña
#################################################################################
# The figure consists of:
#   1. A main ROC-like curve showing the relationship between true positive rates
#      and false positive rates for different effect sizes
#   2. An inset histogram showing the distribution of parameter estimates for
#      three different true effect sizes (null, small, and medium)
#################################################################################
library(here)

########################################################################################
# Part 1: Load simulation results from RData files  
########################################################################################
assign('B', get(load(here("output", "RData-results", "simHypTesting_P40T40_B.RData"))))
assign('B1', get(load(here("output", "RData-results", "simHypTesting_P40T40_B1.RData"))))
assign('B2', get(load(here("output", "RData-results", "simHypTesting_P40T40_B2.RData"))))
# If these files do not exist, you need to run the simulation first
# [repo-root]/demos/simulations/hypothesis_testing/runParallelSeeds.R

# Store the simulation results loaded in the previous step into a single list
estimates <- list("B" = B, "B1" = B1, "B2" = B2)

# Define which simulation results to analyze - using only the available data
look.at <- c("B", "B1", "B2")  # B = null effect, B1 = small effect, B2 = medium effect

# Initialize a vector to store the true effect sizes
true_betas <- rep(NA, length(estimates))


########################################################################################
# Part 2: Compute Bayes factors for the ROC-like curve
########################################################################################
# Define the number of points for z-score threshold evaluation
levelsM <- 1000  

# Define the range of z-score thresholds to evaluate
m <- seq(0, 2, length.out=levelsM)  

# Set up parameters for Bayes factor analysis
levelsM2 <- 10000  # Number of points for Bayes factor threshold evaluation
m2 <- seq(-10, 10, length.out=levelsM2)  # Range of log Bayes factor thresholds
m2[1] <- -Inf  # Set first threshold to negative infinity
m2[levelsM2] <- Inf  # Set last threshold to positive infinity

# Initialize matrices to store results
p <- matrix(NA, ncol=length(look.at), nrow=levelsM)  # For z-score analysis
p2 <- matrix(NA, ncol=length(look.at), nrow=levelsM2)  # For Bayes factor analysis

# Define region of practical equivalence (ROPE) for Bayes factor calculation
epsilon <- 0.1  # Width of the ROPE interval [-epsilon, epsilon]
prior_constant <- pnorm(epsilon) - pnorm(-epsilon)  # Prior probability mass in ROPE

# Initialize Bayes factor storage
BF <- c()

# Loop through each simulation condition
count <- 1
for(b in look.at){
    # Extract results for current condition
    getB <- estimates[[b]]

    # Identify the true effect size
    true_betas[count] <- unique(getB$true[,"betaweight"])

    # Extract parameter estimates and standard deviations
    x <- getB$estimates[,"betaweight"]  # Mean posterior estimates
    sd <- sqrt(getB$variance[,"betaweight"])  # Standard deviations
    check <- x/sd  # Compute z-scores (parameter / SD)
    
    # Extract MCMC samples for Bayes factor calculation
    betas <- getB$beta_chain
    
    # Calculate posterior probability mass in ROPE for each participant
    post_mass <- apply(betas, 3, function(x) mean(x > -epsilon & x < epsilon))
    
    # Calculate log Bayes factors (log of prior/posterior odds)
    this.BF <- log(prior_constant/post_mass)
    this.BF[post_mass==0] <- Inf  # Handle cases where posterior mass is zero
    
    # Store Bayes factors
    BF <- cbind(BF, this.BF)
    
    # Calculate cumulative distributions for different thresholds
    # For z-scores:
    for(i in 1:levelsM){
        p[i,count] <- mean(abs(check) <= m[i], na.rm = TRUE)  # Proportion of z-scores below threshold
    }
    
    # For Bayes factors:
    for(i in 1:levelsM2){
        p2[i,count] <- mean(this.BF <= m2[i], na.rm = TRUE)  # Proportion of BFs below threshold
    }
    
    count <- 1 + count
}

########################################################################################
# Part 3: Make the plot! 
########################################################################################
# Save the plot to a file
fileName <- here("output", "figures", "appendix_HypothesisTesting_ROC.eps")
postscript(fileName, horizontal = FALSE, onefile = FALSE, paper = "special", 
           width = 3, height = 3)

# Set margins to be minimal but equal
par(mai=c(0.5, 0.5, 0.2, 0.2), oma=c(0, 0, 0, 0), bg=NA)#, asp=1)
# Add axes and labels
x_lim <- c(0,1)
axis.cex <- 0.7
x_lab <- seq(0, 1, length.out=7)
beta_colors <- c("#192154", "#5E18F1", "#3C9C3B")

# Initialize empty plot for ROC curve
plot(p2[,2], p2[,1], type="l", ylim=c(0,1), xlim=c(0,1), ann=F, axes=F, lwd=4, col="white")

# Add ROC curves for each condition comparison
# Add a line for the largest effect size
lines(p2[,length(look.at)], p2[,1], col=beta_colors[length(look.at)], lwd=4)  

# Add a line for the smallest, non-null effect size
lines(p2[,2], p2[,1], col=beta_colors[2], lwd=4, lty=2)  

# X-axis
axis(1, x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)  # Tick marks
axis(1, x_lab, round(x_lab, 1), cex.axis=axis.cex, lwd=0, line=-1.35)  # Labels

# Y-axis
axis(2, x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)  # Tick marks
axis(2, x_lab, round(x_lab, 1), cex.axis=axis.cex, lwd=0, line=-0.9, las=2)  # Labels

# Add axis titles
mtext("TRUE positive rate", 1, line=0.8, f=2, cex=0.8)  # X-axis title
mtext("FALSE positive rate", 2, line=1, f=2, cex=0.8)  # Y-axis title

# Add legend elements
lines(c(0.22, 0.25), c(0.85, 0.85), lwd=4, col=beta_colors[2], lty=1)
lines(c(0.28, 0.31), c(0.85, 0.85), lwd=4, col=beta_colors[2], lty=1)
text(0.44, 0.84, bquote(paste("True ", beta, " = ", .(format(true_betas[2], digits=1)))), cex=0.6)
lines(c(0.61, 0.67), c(0.85, 0.85), lwd=4, col=beta_colors[length(look.at)])
text(0.85, 0.84, bquote(paste("True ", beta, " = ", .(format(true_betas[length(look.at)], digits=1)))), cex=0.6)

# Save current plotting parameters for later restoration
old_par <- par(no.readonly = TRUE)

# Create inset plot showing parameter distributions
par(fig = c(0.22, 0.925, 0.1, 0.65), new = TRUE)  # Define inset position

# Extract and plot null effect distribution
getB <- estimates[["B"]]
x <- getB$estimates[,"betaweight"]
x_lim <- c(-0.2, 0.65)
max.Y <- c(max(density(x)$y, density(x)$y, density(x)$y))

# Create histogram for null effect (blue)
hist(x, freq = FALSE, breaks = 50, col="#ADB1C6", border = "#666D95", 
     ann=F, axes = F, ylim = c(0, max.Y*1.4), xlim=x_lim)
lines(density(x), ylim=c(0,8), col=beta_colors[1], lwd=2)

# Add small effect distribution (purple)
getB <- estimates[["B1"]]
x <- getB$estimates[,"betaweight"]
hist(x, freq = FALSE, breaks = 50, col="#C8B6EE", border = "#7757BC", add=T)
lines(density(x), ylim=c(0,8), col=beta_colors[2], lwd=2)

# Add medium effect distribution (green)
getB <- estimates[["B2"]]
x <- getB$estimates[,"betaweight"]
hist(x, freq = FALSE, breaks = 50, col="#E1F4BA", border = "#D2E689", add=T)
lines(density(x), ylim=c(0,8), col=beta_colors[length(look.at)], lwd=2)

# Add x-axis to inset plot
x_lab <- seq(x_lim[1], x_lim[2], length.out=6)
axis(1, x_lab, rep("", length(x_lab)), tck=-0.04, line=0)
axis(1, x_lab, round(x_lab, 1), cex.axis=axis.cex*0.7, lwd=0, line=-1.1)

# Add legend to inset plot
text(-0.17, max.Y*1.05, "True", cex=0.5, col=beta_colors[1])
text(-0.17, max.Y*0.9, bquote(bold(beta)[1] ~ "=" ~ .(format(true_betas[1], digits=1))), 
     cex=0.5, col=beta_colors[1])

text(0.06, max.Y*1.37, "True", cex=0.5, col=beta_colors[2])
text(0.06, max.Y*1.22, bquote(bold(beta)[2] ~ "=" ~ .(format(true_betas[2], digits=1))), 
     cex=0.5, col=beta_colors[2])

text(0.57, max.Y*1.2, "True", cex=0.5, col=beta_colors[3])
text(0.57, max.Y*1.05, bquote(bold(beta)[3] ~ "=" ~ .(format(true_betas[3], digits=1))), 
     cex=0.5, col=beta_colors[3])

mtext(expression(bold(paste("Mean posterior ", bold(beta), " estimated"))), 3, line=-0.2, cex=0.6)

# Restore original plotting parameters
par(old_par)

dev.off()