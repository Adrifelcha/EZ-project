# A function to take difference between the true values and estimates retrieved
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
getDifferences_TRUEvsEST <- function(estimates, parameter_set){
  error <- list("bound" = estimates$bound - parameter_set$bound,
                "nondt" = estimates$nondt - parameter_set$nondt,
                "drift" = estimates$drift - parameter_set$drift,
                "bound_mean" = estimates$bound_mean - parameter_set$bound_mean,
                "bound_sdev" = estimates$bound_sdev - parameter_set$bound_sdev,
                "nondt_mean" = estimates$nondt_mean - parameter_set$nondt_mean,
                "nondt_sdev" = estimates$nondt_sdev - parameter_set$nondt_sdev,
                "drift_mean" = estimates$drift_mean - parameter_set$drift_mean,
                "drift_sdev" = estimates$drift_sdev - parameter_set$drift_sdev)
  return(error)
}