default_priors <- function(Show=TRUE, modelType=NA){
  prior <- data.frame("bound_mean_mean" = 2.25,  "bound_mean_sdev" = 1.00, # 99% density between {-0.07, 4.57}
                      "drift_mean_mean" = 0.00,  "drift_mean_sdev" = 3.00, # 99% density between {-6.9 , 6.9}
                      "nondt_mean_mean" = 0.55,  "nondt_mean_sdev" = 0.25, # 99% density between {-0.03, 1.13}
                      "bound_sdev_shape" = 3.00,  "bound_sdev_rate" = 0.50,   
                      "drift_sdev_shape" = 2.00,  "drift_sdev_rate" = 0.20,
                      "nondt_sdev_shape" = 4.00,  "nondt_sdev_rate" = 0.25)
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