# Load necessary libraries
library(ggplot2)
library(gridExtra)

custom_priors_list <- list(
                      "bound_mean_mean" = 2.5,    "bound_mean_sdev" = 2.00,
                      "drift_mean_mean" = 0.00,    "drift_mean_sdev" = 3.00,
                      "nondt_mean_mean" = 0.5,    "nondt_mean_sdev" = 0.20,
                      "bound_sdev_lower" = 0.01,   "bound_sdev_upper" = 2.00,
                      "drift_sdev_lower" = 0.01,   "drift_sdev_upper" = 2.00,
                      "nondt_sdev_lower" = 0.01,   "nondt_sdev_upper" = 0.50,
                      "betaweight_mean" = 0,       "betaweight_sdev" = 1)

# Define custom truncation list
custom_truncation_list <- list(
        "bound_mean" = c(0.1, ""), "nondt_mean" = c(0.01, ""), "drift_mean" = c("", ""),
        "bound_sdev" = c(0.01, ""), "nondt_sdev" = c(0.01, ""), "drift_sdev" = c(0.01, ""),
        "drift" = c("", ""), "bound" = c(0.0001, ""), "nondt" = c(0.0001, ""), "betaweight" = c("-3", "3"))


generative_uniforms <- list(
        "bound_mean" = c(1, 4), "nondt_mean" = c(0.1, 0.4), "drift_mean" = c(-3, 3),
        "bound_sdev" = c(0.1, 1.5), "nondt_sdev" = c(0.01, 0.17), "drift_sdev" = c(0.1, 1.5))


plot_simulationParameters <- function(custom_priors_list, custom_truncation_list, generative_uniforms,
                                      output_dir = "./", fileName = "generative_vs_priors.pdf"){

        # Extract the mprior parameters defining the hierarchical means
        params <- list(bound = c(custom_priors_list$bound_mean_mean, custom_priors_list$bound_mean_sdev),
                       drift = c(custom_priors_list$drift_mean_mean, custom_priors_list$drift_mean_sdev),
                       nondt = c(custom_priors_list$nondt_mean_mean, custom_priors_list$nondt_mean_sdev))

        # Create the plots
        plots <- list()
        plots[[1]] <- create_normal_plot(mean_val = params$bound[1], sd_val = params$bound[2], title = "Bound Mean", 
                                        truncation = custom_truncation_list$bound_mean,
                                        uniform_range = generative_uniforms$bound_mean,
                                        sdev_range = generative_uniforms$bound_sdev)
        plots[[2]] <- create_normal_plot(mean_val = params$drift[1], sd_val = params$drift[2], title = "Drift Mean", 
                                        truncation = custom_truncation_list$drift_mean,
                                        uniform_range = generative_uniforms$drift_mean,
                                        sdev_range = generative_uniforms$drift_sdev)
        plots[[3]] <- create_normal_plot(mean_val = params$nondt[1], sd_val = params$nondt[2], title = "Non-Decision Time Mean", 
                                        truncation = custom_truncation_list$nondt_mean,
                                        uniform_range = generative_uniforms$nondt_mean,
                                        sdev_range = generative_uniforms$nondt_sdev)

        # Create the PDF
        pdf(here(output_dir, fileName), width = 12, height = 4)
        do.call(grid.arrange, c(plots, ncol = 3))
        dev.off()


}

# Function to create truncated normal distribution plot with uniform range and extreme normals
create_normal_plot <- function(mean_val, sd_val, title, truncation, uniform_range, sdev_range) {
  
  # Set the range of the x-axis to accommodate all distributions
  x_range <- c(min(uniform_range[1] - 3*sdev_range[2], mean_val - 4*sd_val),
               max(uniform_range[2] + 3*sdev_range[2], mean_val + 4*sd_val))
  # Use the x-axis range to define the support of the density curves
  x <- seq(x_range[1], x_range[2], length.out = 200)  
  
  # MAIN DISTRIBUTION: Hierarchical mean
  y <- dnorm(x, mean = mean_val, sd = sd_val)
  
  # Normalize the density to account for truncation
  if(truncation[1] != "" || truncation[2] != "") {
    total_prob <- pnorm(x_range[2], mean_val, sd_val) - pnorm(x_range[1], mean_val, sd_val)
    y <- y/total_prob
  }
  
  data <- data.frame(x = x, y = y)
  
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_line() +
    ggtitle(title) +
    theme_minimal() +
    labs(x = "Value", y = "Density") +
    geom_vline(xintercept = mean_val, linetype = "dashed", color = "blue")
  
  # Add truncation lines if they exist
  if(truncation[1] != "") {
    p <- p + geom_vline(xintercept = as.numeric(truncation[1]), 
                       linetype = "dashed", color = "red")
  }
  if(truncation[2] != "") {
    p <- p + geom_vline(xintercept = as.numeric(truncation[2]), 
                       linetype = "dashed", color = "red")
  }
  
  # Add uniform distribution line
  uniform_height <- 0.1 * max(y)
  p <- p + 
    geom_segment(aes(x = uniform_range[1], y = uniform_height, 
                     xend = uniform_range[2], yend = uniform_height),
                 color = "green", size = 1) +
    geom_vline(xintercept = uniform_range[1], linetype = "dashed", color = "green") +
    geom_vline(xintercept = uniform_range[2], linetype = "dashed", color = "green")
  
  # Add extreme normal distributions
  # For lower extreme
  x_lower <- seq(uniform_range[1] - 3*sdev_range[2], uniform_range[1] + 3*sdev_range[2], length.out = 200)
  # Adjust scaling factors based on parameter
  if(grepl("Non-Decision Time", title)) {
    scale_small <- 0.15  # Reduced from 0.3
    scale_large <- 0.25  # Reduced from 0.5
  } else {
    scale_small <- 0.3
    scale_large <- 0.5
  }
  
  y_lower_small <- dnorm(x_lower, mean = uniform_range[1], sd = sdev_range[1]) * scale_small * max(y)
  y_lower_large <- dnorm(x_lower, mean = uniform_range[1], sd = sdev_range[2]) * scale_large * max(y)
  
  # For upper extreme
  x_upper <- seq(uniform_range[2] - 3*sdev_range[2], uniform_range[2] + 3*sdev_range[2], length.out = 200)
  y_upper_small <- dnorm(x_upper, mean = uniform_range[2], sd = sdev_range[1]) * scale_small * max(y)
  y_upper_large <- dnorm(x_upper, mean = uniform_range[2], sd = sdev_range[2]) * scale_large * max(y)
  
  # Add the extreme normal distributions to the plot
  p <- p +
    # Lower extreme
    geom_line(data = data.frame(x = x_lower, y = y_lower_small), 
              aes(x = x, y = y), color = "purple", alpha = 0.7, size = 1) +
    geom_line(data = data.frame(x = x_lower, y = y_lower_large), 
              aes(x = x, y = y), color = "purple", alpha = 0.5, size = 1) +
    # Upper extreme
    geom_line(data = data.frame(x = x_upper, y = y_upper_small), 
              aes(x = x, y = y), color = "purple", alpha = 0.7, size = 1) +
    geom_line(data = data.frame(x = x_upper, y = y_upper_large), 
              aes(x = x, y = y), color = "purple", alpha = 0.5, size = 1)
  
  # Add blue horizontal lines for the ranges from custom_truncation_list
  # Get the appropriate range based on the title
  if(grepl("Bound", title)) {
    range_vals <- c(as.numeric(custom_truncation_list$bound[1]), 
                   if(custom_truncation_list$bound[2] == "") uniform_range[2] 
                   else as.numeric(custom_truncation_list$bound[2]))
  } else if(grepl("Drift", title)) {
    range_vals <- as.numeric(custom_truncation_list$drift)
  } else if(grepl("Non-Decision Time", title)) {
    range_vals <- c(as.numeric(custom_truncation_list$nondt[1]), 
                   if(custom_truncation_list$nondt[2] == "") uniform_range[2] 
                   else as.numeric(custom_truncation_list$nondt[2]))
  }
  
  # Add the blue horizontal line at the middle height
  p <- p + 
    geom_segment(aes(x = range_vals[1], y = max(y) * 0.5, 
                     xend = range_vals[2], yend = max(y) * 0.5),
                 color = "blue", size = 1) +
    geom_vline(xintercept = range_vals[1], linetype = "dashed", color = "blue") +
    geom_vline(xintercept = range_vals[2], linetype = "dashed", color = "blue")
  
  # Add annotations
  p <- p + annotate("text", x = mean_val, y = max(y), 
                    label = paste("mean =", round(mean_val, 2), "\nsd =", round(sd_val, 2)),
                    hjust = -0.1, vjust = 1)
  
  return(p)
}