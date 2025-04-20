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
if(file.exists(here("output", "RData-results", "simHypTesting_P40T80_B.RData"))){
          assign('B', get(load(here("output", "RData-results", "simHypTesting_P40T80_B.RData"))))
          assign('B1', get(load(here("output", "RData-results", "simHypTesting_P40T80_B1.RData"))))
          assign('B2', get(load(here("output", "RData-results", "simHypTesting_P40T80_B2.RData"))))
}else{
     cat("Error: The RData files do not exist. Please run the simulation first.\n")
     cat("Run the simulation using the following command:\n")
     cat("Rscript ../runParallelSeeds.R\n")     
}

# Store the simulation results loaded in the previous step into a single list
estimates <- list("B" = B, "B1" = B1, "B2" = B2)

# Define which simulation results to analyze
look.at <- c("B", "B1", "B2")  # B = null effect, B1 = small effect, B2 = medium effect

# Initialize a vector to store the true effect sizes
true_betas <- rep(NA, length(estimates))


########################################################################################
# Part 2: Compute Bayes factors for the ROC-like curve
########################################################################################
levelsM <- 1000  # Number of points for Bayes factor threshold evaluation
m <- seq(-10, 10, length.out=levelsM)  # Range of log Bayes factor thresholds
m[1] <- -Inf  # Set first threshold to negative infinity
m[levelsM] <- Inf  # Set last threshold to positive infinity

# Define region of practical equivalence (ROPE) for Bayes factor calculation
epsilon <- 0.1  # Width of the ROPE interval ([+/-] epsilon)
prior_constant <- pnorm(epsilon) - pnorm(-epsilon)  # Prior probability mass in ROPE

# Initialize empty objects to store results
BF <- c()
p <- matrix(NA, ncol=length(look.at), nrow=levelsM)  # For Bayes factor analysis

# Loop through each simulated beta value
count <- 1
for(b in look.at){
          # Extract results for this beta value 
          getB <- estimates[[b]]          
          true_betas[count] <- unique(getB$true[,"betaweight"])          
          betas <- getB$beta_chain                    

          # Calculate posterior probability mass in ROPE for each participant
          post_mass <- apply(betas, 3, function(x) mean(x > -epsilon & x < epsilon))
          
          # Calculate log Bayes factors (log of prior/posterior odds)
          this.BF <- log(prior_constant/post_mass)
          this.BF[post_mass==0] <- Inf       # Handle cases where posterior mass is zero          
          BF <- cbind(BF, this.BF)           # Store Bayes factors
          
          # Calculate cumulative distributions (for Bayes factors):
          for(i in 1:levelsM){
               p[i,count] <- mean(this.BF <= m[i], na.rm = TRUE)  # Proportion of BFs below threshold
          }
          
     count <- 1 + count
}

########################################################################################
# Part 3: Make the ROC plot with an inset showing the distribution of parameter estimates 
########################################################################################
# Save the plot to a file
makeROCPlot <- function(){          
          par(mai=c(0.4, 0.4, 0.1, 0.1), oma=c(0, 0, 0, 0), bg=NA)
          
          # Plotting variables
          x_lim <- c(0,1)
          axis.cex <- 0.7
          x_lab <- seq(0, 1, length.out=7)
          beta_colors <- c("#555555", "#2ea02d", "#de8520")
          trueBeta_colors <- c("#333030", "#296728", "#a64c0e")

          ########################################
          # ROC plot
          ########################################
          # Start with an empty plot
          plot(0.5, 0.5, ylim=c(0,1), xlim=c(0,1), ann=F, axes=F, lwd=4, type="n")
          # Add ROC curves
          # Curve 1: Largest effect size
          lines(p[,length(look.at)], p[,1], col=beta_colors[length(look.at)], lwd=4)  
          # Curve 2: Smallest, non-null effect size
          lines(p[,2], p[,1], col=beta_colors[2], lwd=4, lty=2)  
          # Add tick marks and labels
          axis(1, x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)  # Tick marks
          axis(1, x_lab, round(x_lab, 1), cex.axis=axis.cex, lwd=0, line=-1.35)  # Labels
          axis(2, x_lab, rep("", length(x_lab)), tck=-0.02, line=-0.3)  # Tick marks
          axis(2, x_lab, round(x_lab, 1), cex.axis=axis.cex, lwd=0, line=-0.9, las=2)  # Labels
          # Add axis titles
          mtext("TRUE positive rate", 1, line=0.8, f=2, cex=0.8)  # X-axis title
          mtext("FALSE positive rate", 2, line=1, f=2, cex=0.8)  # Y-axis title
          # Add legend elements
          lines(c(0.24, 0.26), c(0.84, 0.84), lwd=5, col=beta_colors[2], lty=1)
          lines(c(0.29, 0.31), c(0.84, 0.84), lwd=5, col=beta_colors[2], lty=1)
          text(0.46, 0.84, bquote(paste("True ", beta, " = ", .(format(true_betas[2], digits=1)))), cex=0.75)
          lines(c(0.65, 0.71), c(0.84, 0.84), lwd=4, col=beta_colors[length(look.at)])
          text(0.85, 0.84, bquote(paste("True ", beta, " = ", .(format(true_betas[length(look.at)], digits=1)))), cex=0.75)

          # Save current plotting parameters for later restoration
          old_par <- par(no.readonly = TRUE)
          ########################################
          # Inset plot
          ########################################
          par(fig = c(0.22, 0.925, 0.1, 0.65), new = TRUE)  # Define inset position

          # Histogram 1: Null effect distribution
          getB <- estimates[["B"]]
          x <- getB$estimates[,"betaweight"]
          x_lim <- c(-0.2, 0.65)
          max.Y <- c(max(density(x)$y, density(x)$y, density(x)$y))
          hist(x, freq = FALSE, breaks = 50, col="#c9c9c9", border = "#a4a4a4", 
               ann=F, axes = F, ylim = c(0, max.Y*1.4), xlim=x_lim)
          lines(density(x), ylim=c(0,8), col=beta_colors[1], lwd=2)

          # Histogram 2: Small effect distribution
          getB <- estimates[["B1"]]
          x <- getB$estimates[,"betaweight"]
          hist(x, freq = FALSE, breaks = 50, col="#E1F4BA", border = "#92c291", add=T)
          lines(density(x), ylim=c(0,8), col=beta_colors[2], lwd=2)

          # Histogram 3: Largest effect distribution
          getB <- estimates[["B2"]]
          x <- getB$estimates[,"betaweight"]
          hist(x, freq = FALSE, breaks = 50, col="#ede0d2", border = "#e3a96b", add=T)
          lines(density(x), ylim=c(0,8), col=beta_colors[length(look.at)], lwd=2)

          # Finish plot
          x_lab <- seq(x_lim[1], x_lim[2], length.out=6)
          axis(1, x_lab, rep("", length(x_lab)), tck=-0.04, line=0)
          axis(1, x_lab, round(x_lab, 1), cex.axis=axis.cex*0.7, lwd=0, line=-1.1)
          # Add legend and title to inset plot
          trueBeta_label_cex <- 0.75
          text(-0.15, max.Y*1.05, "True", cex=trueBeta_label_cex-0.05, col=trueBeta_colors[1])
          text(-0.15, max.Y*0.86, bquote(bold(beta)[1] ~ "=" ~ .(format(true_betas[1], digits=1))), 
               cex=trueBeta_label_cex, col=trueBeta_colors[1])
          text(0.2, max.Y*1.3, "True", cex=trueBeta_label_cex-0.05, col=trueBeta_colors[2])
          text(0.2, max.Y*1.1, bquote(bold(beta)[2] ~ "=" ~ .(format(true_betas[2], digits=1))), 
               cex=trueBeta_label_cex, col=trueBeta_colors[2])
          text(0.57, max.Y*1.14, "True", cex=trueBeta_label_cex-0.05, col=trueBeta_colors[3])
          text(0.57, max.Y*0.95, bquote(bold(beta)[3] ~ "=" ~ .(format(true_betas[3], digits=1))), 
               cex=trueBeta_label_cex, col=trueBeta_colors[3])
          mtext(expression(bold(paste("Mean posterior ", bold(beta), " estimated"))), 3, line=-0.2, cex=0.8)
          # Restore original plotting parameters
          par(old_par)
}

postscript(here("output", "figures", "in-paper", "paper_hypothesisAppendix_ROC.eps"), horizontal = FALSE, onefile = FALSE, paper = "special",
           width = 3.5, height = 3)
makeROCPlot()
dev.off()

png(here("output", "figures", "in-paper", "paper_hypothesisAppendix_ROC.png"), width = 3.5, height = 3, units = "in", res = 300)
makeROCPlot()
dev.off()

pdf(here("output", "figures", "in-paper", "paper_hypothesisAppendix_ROC.pdf"), width = 3.5, height = 3)
makeROCPlot()
dev.off()