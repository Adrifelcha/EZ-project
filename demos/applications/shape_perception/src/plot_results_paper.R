###########################################################################################
# Load results and define plotting variables
############################################################################################
# Load packages
library(here)
# Load custom plotting function
source(here("src", "plot_VerticalHist.R")) 
# Load results
results_at <- here("output", "RData-results", "demo_shape_results.RData")
if(file.exists(results_at)){
  load(results_at)
} else {
  source(here("demos", "applications", "shape_perception", "src", "run_hypoTesting-example.R"))
}

###########################################################################################
# Plotting file description:
# This R script generates figures used in the paper
############################################################################################
# Custom plotting functions and variables

driftPred <- drift_pred[,1:4]
colnames(driftPred) <- c("QualConvex","QuantConvex","QualConcav","QuantConcav")

axisCol <- "black"
curve.gamma1 <- "#FDBB86"
line.gamma1  <- "#D57004"
curve.gamma2 <- "#C4FFCA"
line.gamma2  <- "#2D9033"
curve.gamma3 <- "#81A0D7"
line.gamma3  <- "#103C8A"
cutLine.color <- "#DF1919"

x_lim <- c(-1.8,1.8)
x_lab <- round(seq(-1.5,1.5,length.out=5),1)
max.Y <- c(max(density(gamma[,1])$y,density(gamma[,2])$y,density(gamma[,3])$y))

###################################################################
# Figure 1: Gamma posteriors and drift rate predictions
###################################################################
plot_gamma_posteriors_and_drift <- function() {
  # Set up layout with 3 rows, 2 columns, with drift predictions plot spanning all rows
  layout_matrix <- matrix(c(1, 4, 2, 4, 3, 4), nrow = 3, ncol = 2, byrow = TRUE)
  # Make the drift predictions column wider
  layout(layout_matrix, widths = c(1, 1.8), heights = c(1, 1, 1))
  par(mai=c(0.2,0,0.1,0), oma= c(0.3,0.2,0.7,0), bg=NA)
  
  # Set up plotting format variables
  fig_title_sep <- 0.35    # Separation between figure title and plot

  hist_xaxis_sep <- -0.1   # Horizontal line separation from plot
  hist_xaxis_tck <- -0.07  # Tick mark length
  hist_xaxis_cex <- 0.6    # X-axis value size
  hist_xaxis_lab <- -1.1   # X-axis values separation from plot
  hist_xlabel_cex <- 0.7   # X-axis label size
  hist_xlabel_sep <- 0.8   # X-axis label separation
  hist_text_cex <- 0.9     # Text size
  hist_text_inline_sep <- 0.35 # Separation between text lines
  hist_text_top <- 1.5     # Text top position
  hist1_text_x <- 1        # Text position on x axis
  hist2_text_x <- -0.9     # Text position on x axis
  hist3_text_x <- 0.8      # Text position on x axis


  ###################################################################
  # First row, left column: Gamma 1 : Main effect of a Quantitative change
  ###################################################################
  hist(gamma[,1], freq = FALSE, breaks = 50, col=curve.gamma1, border = curve.gamma1, 
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  mtext("Regression coefficients",side=3,line=fig_title_sep, cex=0.8, f=2)
  lines(density(gamma[,1]), lwd=2, col=line.gamma1)  
  mtext(expression(paste(gamma[1])),side=1,line=hist_xlabel_sep, cex=hist_xlabel_cex)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=hist_xaxis_sep)
  axis(1,x_lab, rep("", length(x_lab)), tck=hist_xaxis_tck, line=hist_xaxis_sep)
  axis(1,x_lab, x_lab, cex.axis=hist_xaxis_cex, lwd=0, line=hist_xaxis_lab)
  text(hist1_text_x,hist_text_top, "Effect of a", cex=hist_text_cex, f=2)
  text(hist1_text_x,hist_text_top-hist_text_inline_sep, "Quantitative", cex=hist_text_cex, f=2)
  text(hist1_text_x,hist_text_top-2*hist_text_inline_sep, "change", cex=hist_text_cex, f=2)
  
  ###################################################################
  # Second row, left column: Gamma 2 : Main effect of a change in Concavity
  ###################################################################
  hist(gamma[,2], freq = FALSE, breaks = 50, col=curve.gamma2, border = curve.gamma2,
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  lines(density(gamma[,2]), lwd=2, col=line.gamma2)  
  mtext(expression(paste(gamma[2])),side=1,line=hist_xlabel_sep, cex=hist_xlabel_cex)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=hist_xaxis_sep)
  axis(1,x_lab, rep("", length(x_lab)), tck=hist_xaxis_tck, line=hist_xaxis_sep)
  axis(1,x_lab, x_lab, cex.axis=hist_xaxis_cex, lwd=0, line=hist_xaxis_lab)  
  text(hist2_text_x,hist_text_top, "Effect of a", cex=hist_text_cex, f=2)
  text(hist2_text_x,hist_text_top-hist_text_inline_sep, "change in", cex=hist_text_cex, f=2)
  text(hist2_text_x,hist_text_top-2*hist_text_inline_sep, "Concavity", cex=hist_text_cex, f=2)
  
  ###################################################################
  # Third row, left column: Gamma 3 : Interaction effect
  ###################################################################
  hist(gamma[,3], freq = FALSE, breaks = 50, col=curve.gamma3, border = curve.gamma3, 
       ann=F, axes = F, ylim = c(0, max.Y), xlim=x_lim)
  lines(density(gamma[,3]), lwd=2, col=line.gamma3)  
  mtext(expression(bold(paste(gamma[3]))),side=1,line=hist_xlabel_sep, cex=hist_xlabel_cex)
  abline(v=0,lty=2,col=cutLine.color, lwd=2)
  axis(1,x_lim, c("",""), tck=-0, line=hist_xaxis_sep)
  axis(1,x_lab, rep("", length(x_lab)), tck=hist_xaxis_tck, line=hist_xaxis_sep)
  axis(1,x_lab, x_lab, cex.axis=hist_xaxis_cex, lwd=0, line=hist_xaxis_lab)  
  text(hist3_text_x,hist_text_top, "Interaction", cex=hist_text_cex, f=2)
  text(hist3_text_x,hist_text_top-hist_text_inline_sep, "effect", cex=hist_text_cex, f=2)
  
  ###################################################################
  # Right column (spanning all rows): Posterior drift rate distributions
  ###################################################################
  data <- drift_pred
  binWidth = 0.1
  
  binStarts <- c(0.9,1.1,1.6,1.8)
  binMids <- binStarts + binWidth / 2
  
  DOYrange <- range(data)
  ## Get the histogram obects
  histList <- apply(data, 2, function(x, hCol) hist(x, plot = FALSE))
  
  means <- apply(data, 2, mean)
  CI <- apply(data, 2, quantile, prob=c(0.025,0.975))
  shadeCI <- list()
  for(i in 1:ncol(CI)){
    keep <- (data[,i] > CI[1,i]) & (data[,i] < CI[2,i])
    nBreaks <- sum(histList[[i]]$breaks >= CI[1,i] & histList[[i]]$breaks <= CI[2,i])
    partial <- hist(data[keep,i], breaks = nBreaks, plot = FALSE)
    shadeCI <- append(shadeCI, list(partial))
  }
  
  ## Plotting
  xlim <- c(0.5,2)
  ylim <- c(-1.2,2)
  xlabs <- c("Convexity", "Concavity")
  fillCol <- c("#42B6EC", "#0D41D5", "#42B6EC", "#0D41D5")
  CIcolor <- rep("#CAC7DA",4)
  

  vert_yaxis_sep <- -4   # Vertical line separation from plot
  vert_yaxis_tck <- -0.028 # Tick mark length
  vert_yaxis_cex <- hist_xlabel_cex   # Y-axis value size
  vert_yaxis_lab <- -4.3  # Y-axis values separation from plot
  vert_ylabel_cex <- 0.8  # Y-axis label size
  vert_ylabel_sep <- -2.2  # Y-axis label separation
  vert_xlabel_at <- c(1,1.7)
  label_line_length <- c(0.8,0.875)
  factor_label_cex <- hist_text_cex

  plot(c(0, 5), DOYrange, type = "n", xlim=xlim, ylim=ylim,
       ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i")
  mtext("Drift rate per condition",side=3,line=fig_title_sep, cex=0.8, f=2)
  axis(1, mean(vert_xlabel_at), "Change type", cex.axis = factor_label_cex, col = NA, line=-0.3, f=2)
  axis(1, vert_xlabel_at, xlabs, cex.axis = hist_xlabel_cex, col = NA, line=-0.8, f=1)
  axis(1, vert_xlabel_at, c("",""), cex.axis = 0.7, tck=-0.02)
  y.seq = format(round(seq(-1.2,2,length.out=7),digits = 1), nsmall = 1)
  axis(2,ylim, c("",""), tck=-0, line=vert_yaxis_sep)
  axis(2,y.seq, rep("", length(y.seq)), tck=vert_yaxis_tck, line=vert_yaxis_sep)
  axis(2,y.seq, y.seq, cex.axis=vert_yaxis_cex, lwd=0, line=vert_yaxis_lab, las=2)
  
  mtext(side = 2, outer = F, line = vert_ylabel_sep, expression(paste(nu^pred)), cex = vert_ylabel_cex)
  #box(bty = "n", col = axisCol)
  
  biggestDensity <- max(unlist(lapply(histList, function(h){max(h[[4]])})))
  xscale <- binWidth * .9 / biggestDensity
  
  ## Plot the histograms
  for (i in 1:4) {
    X <- binStarts[i]
    VerticalHist(x = X, xscale = xscale, xwidth = binWidth, 
                 hist= histList[[i]], fillCol = "black")
    VerticalHist(x = X, xscale = xscale*0.8, xwidth = binWidth, 
                 hist= shadeCI[[i]], fillCol = fillCol[i])
    points(X, means[i], pch=18, cex=0.5)
    lines(x=c(X-0.05, X+0.05), y=c(means[i],means[i]), lwd=1)
  }
  lines(binStarts[c(1,3)], means[c(1,3)], lwd=1, lty=2)
  lines(binStarts[c(2,4)], means[c(2,4)], lwd=1, lty=2)
  text(1.1,1.78,"Change quality", cex=factor_label_cex, f=2)
  lines(label_line_length,c(1.55,1.55), lwd=3, col="#42B6EC")
  text(0.995,1.55,"Qualitative", cex=0.7, f=1)
  lines(label_line_length,c(1.35,1.35), lwd=3, col="#0D41D5")
  text(1.01,1.35,"Quantitative", cex=0.7, f=1)
}

###################################################################
# Generate all figures in PNG, PDF, and EPS formats
###################################################################

# Figure 1: Gamma posteriors and drift rate predictions
# PNG version

width <- 5
height <- 2.5

png(file = here("output", "figures", "in-paper", "paper_shapeExample_gammas.png"), width = width, height = height, units="in", res=300)
plot_gamma_posteriors_and_drift()
dev.off()

# PDF version
pdf(file = here("output", "figures", "in-paper", "paper_shapeExample_gammas.pdf"), width = width, height = height)
plot_gamma_posteriors_and_drift()
dev.off()

# EPS version
setEPS()
postscript(file = here("output", "figures", "in-paper", "paper_shapeExample_gammas.eps"), width = width, height = height)
plot_gamma_posteriors_and_drift()
dev.off()