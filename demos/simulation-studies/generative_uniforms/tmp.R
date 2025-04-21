custom_priors_list <- list(
                      "bound_mean_mean" = 2.5,    "bound_mean_sdev" = 1.00,
                      "drift_mean_mean" = 0.00,    "drift_mean_sdev" = 3.00,
                      "nondt_mean_mean" = 0.55,    "nondt_mean_sdev" = 0.25,
                      "bound_sdev_lower" = 0.01,   "bound_sdev_upper" = 2.00,
                      "drift_sdev_lower" = 0.01,   "drift_sdev_upper" = 2.00,
                      "nondt_sdev_lower" = 0.01,   "nondt_sdev_upper" = 0.50,
                      "betaweight_mean" = 0,       "betaweight_sdev" = 1)

# Create a 2x3 grid of normal distributions
library(ggplot2)
library(gridExtra)

# Extract the first six elements into 3 pairs of mean/sdev
params <- list(
  bound = c(custom_priors_list$bound_mean_mean, custom_priors_list$bound_mean_sdev),
  drift = c(custom_priors_list$drift_mean_mean, custom_priors_list$drift_mean_sdev),
  nondt = c(custom_priors_list$nondt_mean_mean, custom_priors_list$nondt_mean_sdev)
)

# Function to create normal distribution plot
create_normal_plot <- function(mean_val, sd_val, title) {
  x <- seq(mean_val - 3*sd_val, mean_val + 3*sd_val, length.out = 100)
  y <- dnorm(x, mean = mean_val, sd = sd_val)
  data <- data.frame(x = x, y = y)
  
  ggplot(data, aes(x = x, y = y)) +
    geom_line() +
    ggtitle(title) +
    theme_minimal() +
    labs(x = "Value", y = "Density")
}

# Create the plots
plots <- list()
plots[[1]] <- create_normal_plot(params$bound[1], params$bound[2], "Bound Mean") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")
plots[[2]] <- create_normal_plot(params$drift[1], params$drift[2], "Drift Mean")
plots[[3]] <- create_normal_plot(params$nondt[1], params$nondt[2], "Non-Decision Time Mean") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")

# Create the PDF
pdf(here("demos", "simulation-studies", "generative_uniforms", "normal_distributions.pdf"), width = 10, height = 4)
do.call(grid.arrange, c(plots, ncol = 3))
dev.off()

cat("PDF created: normal_distributions.pdf\n")