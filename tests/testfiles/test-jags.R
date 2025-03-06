library(testthat)
library(here)

# Source helper paths first
source(file.path(here("tests"), "helper-paths.R"))

test_that("[JAGS] Rhat calculation correctly assesses chain convergence", {
  source(CORE_FUNCTIONS$rhat)
  
  # Create mock chain data with known convergence
  set.seed(123)
  n_samples <- 1000
  mock_chains <- matrix(
    c(
      rnorm(n_samples, mean = 0, sd = 1),  # Chain 1
      rnorm(n_samples, mean = 0, sd = 1)   # Chain 2
    ),
    ncol = 2
  )
  
  # Calculate Rhat
  rhat_value <- getRhat(mock_chains)
  
  # Test expectations
  expect_type(rhat_value, "double")
  expect_gte(rhat_value, 0)
  expect_lte(rhat_value, 2)  # Well-converged chains should have Rhat close to 1
})

test_that("[JAGS] Model writer creates valid JAGS model file", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$write_JAGSmodel)
  
  # Get default priors with specific model type
  priors <- default_priors(
    Show = FALSE,
    modelType = "simple"
  )
  
  # Write model file with specific parameters
  write_JAGSmodel(
    priors = priors,
    modelType = "simple",
    criterion = "nondt",
    modelFile = OUTPUT_FILES$jags_model
  )
  
  # Check if file exists
  expect_true(file.exists(OUTPUT_FILES$jags_model))
  
  # Read the generated file to verify content
  model_content <- readLines(OUTPUT_FILES$jags_model)
  expect_true(length(model_content) > 0)
  expect_true(any(grepl("model", model_content)))
  
  # Clean up
  if(file.exists(OUTPUT_FILES$jags_model)) {
    unlink(OUTPUT_FILES$jags_model)
  }
}) 