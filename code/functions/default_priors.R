default_priors <- function(Show=TRUE, modelType=NA){
  prior <- data.frame("bound_mean_mean" = 1.50,  "bound_mean_sdev" = 0.20,
                      "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 0.50,
                      "nondt_mean_mean" = 0.30,  "nondt_mean_sdev" = 0.06,
                      "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,
                      "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
                      "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25)
  if(modelType!="hierarchical"){
            prior$betaweight_lower = -2.00
            prior$betaweight_upper = 2.00
  }
  if(Show){       show_priors(prior)           }
  return(prior)
}