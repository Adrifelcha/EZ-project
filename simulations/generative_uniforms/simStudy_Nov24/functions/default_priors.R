#################################################################
# Priors used on 
#################################################################
default_priors <- function(Show=TRUE, modelType=NA){
  prior <- data.frame("bound_mean_mean" = 2.25,  "bound_mean_sdev" = 1.00, 
                      "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 3.00, 
                      "nondt_mean_mean" = 0.55,  "nondt_mean_sdev" = 0.25, 
                      "bound_sdev_lower" = 0.01,  "bound_sdev_upper" = 2,   
                      "drift_sdev_lower" = 0.01,  "drift_sdev_upper" = 2,
                      "nondt_sdev_lower" = 0.01,  "nondt_sdev_upper" = 0.5)
  if(modelType!="hierarchical"){
            prior$betaweight_mean = 0
            prior$betaweight_sdev = 1
  }
  if(Show){       show_priors(prior)           }
  return(prior)
}