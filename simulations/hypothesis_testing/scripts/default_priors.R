#################################################################
# Priors used on 
#################################################################
default_priors <- function(Show=TRUE){
  #prior <- data.frame("bound_mean_mean" = 3.00,  "bound_mean_sdev" = 1.00, 
  #                    "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 1.50, 
  #                    "nondt_mean_mean" = 0.40,  "nondt_mean_sdev" = 0.15, 
  #                    "bound_sdev_lower" = 0.01,  "bound_sdev_upper" = 0.50,   
  #                    "drift_sdev_lower" = 0.01,  "drift_sdev_upper" = 0.50,
  #                    "nondt_sdev_lower" = 0.01,  "nondt_sdev_upper" = 0.50,
  #                    "betaweight_mean" = 0.00, "betaweight_sdev" = 1.00)
  prior <- data.frame("bound_mean_mean" = 1.5,  "bound_mean_sdev" = 0.2, 
                      "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 0.50, 
                      "nondt_mean_mean" = 0.30,  "nondt_mean_sdev" = 0.06, 
                      "bound_sdev_lower" = 0.10,  "bound_sdev_upper" = 0.40,   
                      "drift_sdev_lower" = 0.20,  "drift_sdev_upper" = 0.40,
                      "nondt_sdev_lower" = 0.025,  "nondt_sdev_upper" = 0.25,
                      "betaweight_mean" = 0.00, "betaweight_sdev" = 1.00)
  if(Show){       show_priors(prior)           }
  return(prior)
}

