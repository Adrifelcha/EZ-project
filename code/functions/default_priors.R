default_priors <- function(Show=TRUE, modelType=NA){
  prior <- data.frame("bound_mean_mean" = 2.25,  "bound_mean_sdev" = 0.80, # 99% density between {0.38, 4.11}
                      "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 2.15, # 99% density between {-5 , 5}
                      "nondt_mean_mean" = 0.55,  "nondt_mean_sdev" = 0.20, # 99% density between {0.08, 1.01}
                      "bound_sdev_lower" = 0.05, "bound_sdev_upper" = 1,   
                      "drift_sdev_lower" = 0.05, "drift_sdev_upper" = 2,
                      "nondt_sdev_lower" = 0.01, "nondt_sdev_upper" = 0.25)
  # prior <- data.frame("bound_mean_mean" = 1.50,  "bound_mean_sdev" = 0.20, # 95% density between {1.1 , 1.89}
  #                     "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 0.50, # 95% density between {-0.98 , 0.98}
  #                     "nondt_mean_mean" = 0.30,  "nondt_mean_sdev" = 0.06, # 98% density between {0.18, 0.41}
  #                     "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,   
  #                     "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
  #                     "nondt_sdev_lower" = 0.025, "nondt_sdev_upper" = 0.25)
  if(modelType!="hierarchical"){
            prior$betaweight_mean = 0
            prior$betaweight_sdev = 1
  }
  if(Show){       show_priors(prior)           }
  return(prior)
}