library(testthat)
library(here)

# Source helper paths first
source(file.path(here("tests"), "helper-paths.R"))

test_that("[Core] generate_trial produces valid trial data", {
  source(CORE_FUNCTIONS$generate_trial)
  # Test with specific parameters
  trial <- generate_trial(
    a = 2,           # boundary separation
    v = 2,           # increase drift rate to ensure boundary is reached
    dt = 0.001,      # time step size
    max_steps = 1e6  # increased max steps
  )
  expect_type(trial, "list")
  expect_named(trial, c("RT", "C"))
  expect_true(trial$RT > 0)
  expect_true(abs(trial$C) > 0) # Should have some movement towards boundary
})

test_that("[Core] default_priors returns correct prior structure", {
  source(CORE_FUNCTIONS$default_priors)
  priors <- default_priors(
    Show = FALSE,
    modelType = "simple"
  )
  expect_type(priors, "list")
  expect_true(all(c("bound_mean_mean", "drift_mean_mean", "nondt_mean_mean") %in% names(priors)))
})

test_that("[Core] default_inits creates correct number of chains", {
  source(CORE_FUNCTIONS$default_inits)
  inits <- default_inits(
    n.chains = 3,
    nParticipants = 1
  )
  expect_type(inits, "list")
  expect_length(inits, 3) # Should have one list per chain
})

test_that("[Core] Parameter sampling generates valid parameter sets", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  
  # Get priors first
  priors <- default_priors(
    Show = FALSE,
    modelType = "simple"
  )
  
  # Sample parameters
  params <- sample_parameters(
    priors = priors,
    nPart = 1,
    criterion = "nondt",  # Specify criterion
    modelType = "simple",  # Add modelType parameter
    Show = FALSE,
    X = c(0,0,0,0,1,1,1,1)
  )
  
  expect_type(params, "list")
  expect_length(params$bound, 1)
  expect_length(params$drift, 1)
  expect_length(params$nondt, 1)
})

test_that("[Core] Data sampling produces correct trial data structure", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  source(CORE_FUNCTIONS$sample_data)
  source(CORE_FUNCTIONS$generate_dataset)
  source(CORE_FUNCTIONS$generate_trial)
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Get priors
  priors <- default_priors(
    Show = FALSE,
    modelType = "simple"
  )
  
  nPart = 2
  nTrials = 10

  # Sample parameters
  params <- sample_parameters(
    priors = priors,
    nPart = nPart,
    criterion = "nondt",  # Specify criterion
    modelType = "simple",  # Add modelType parameter
    Show = FALSE,
    X = c(0,0,0,0,1,1,1,1)
  )
  
  # Generate data
  data <- sample_data(
    nPart = nPart,
    nTrials = nTrials,  # Explicitly use nTrials
    parameter_set = params,
    nTrialsPerCondition = NA  # Explicitly set to NA to ensure we get 3-column output
  )
  
  # Test data structure (deterministic aspects)
  expect_true(is.matrix(data))
  expect_equal(ncol(data), 3)
  expect_equal(colnames(data), c("sub", "rt", "accuracy"))
  expect_equal(nrow(data), nPart * nTrials)
  expect_true(all(data[,"sub"] %in% 1:nPart))
  
  # Test data values (stochastic aspects with reasonable bounds)
  expect_true(all(data[,"rt"] > 0))  # RTs should always be positive
  expect_true(all(data[,"rt"] < 10))  # RTs shouldn't be unreasonably large
  expect_true(all(data[,"accuracy"] %in% c(0, 1)))  # Binary accuracy
  
  # Test reasonable success rate (not too extreme)
  accuracy_rate <- mean(data[,"accuracy"])
  expect_true(accuracy_rate > 0.2 && accuracy_rate < 0.8,
              "Accuracy rate should be reasonable (between 20% and 80%)")
})

test_that("[Core] Data sampling - simple trials produces correct structure", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  source(CORE_FUNCTIONS$sample_data)
  source(CORE_FUNCTIONS$generate_dataset)
  source(CORE_FUNCTIONS$generate_trial)
  
  set.seed(42)
  
  priors <- default_priors(Show = FALSE, modelType = "simple")
  nPart <- 2
  nTrials <- 10
  
  params <- sample_parameters(
    priors = priors,
    nPart = nPart,
    criterion = "nondt",
    modelType = "simple",
    Show = FALSE,
    X = c(0,0,0,0,1,1,1,1)
  )
  
  data <- sample_data(
    nPart = nPart,
    nTrials = nTrials,
    parameter_set = params,
    nTrialsPerCondition = NA
  )
  
  # Structure tests
  expect_true(is.matrix(data))
  expect_equal(ncol(data), 3)
  expect_equal(colnames(data), c("sub", "rt", "accuracy"))
  expect_equal(nrow(data), nPart * nTrials)
  expect_true(all(data[,"sub"] %in% 1:nPart))
  
  # Value tests
  expect_true(all(data[,"rt"] > 0))
  expect_true(all(data[,"rt"] < 10))
  expect_true(all(data[,"accuracy"] %in% c(0, 1)))
  
  # Distribution tests
  accuracy_rate <- mean(data[,"accuracy"])
  expect_true(accuracy_rate > 0.2 && accuracy_rate < 0.8)
})

test_that("[Core] Data sampling - condition trials produces correct structure", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  source(CORE_FUNCTIONS$sample_data)
  source(CORE_FUNCTIONS$generate_dataset)
  source(CORE_FUNCTIONS$generate_trial)
  
  set.seed(123)  # Different seed to get positive drift rates
  
  # Use custom priors with larger drift rates and boundary separation
  priors <- data.frame(
    "bound_mean_mean"  = 2.50, "bound_mean_sdev"  = 0.20,  # Large boundary separation
    "drift_mean_mean"  = 2.00, "drift_mean_sdev"  = 0.50,  # Keep drift rate the same
    "nondt_mean_mean"  = 0.30, "nondt_mean_sdev"  = 0.06, 
    "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,   
    "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
    "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25,
    "betaweight_mean"  = 0.00, "betaweight_sdev"  = 1.00
  )
  
  nPart <- 2
  nTrialsPerCondition <- 5
  
  params <- sample_parameters(
    priors = priors,
    nPart = nPart,
    criterion = "drift",  # Changed from nondt
    modelType = "simple",
    Show = FALSE,
    X = c(0,0,1,1),  # X vector for 2 participants
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  data <- sample_data(
    nPart = nPart,
    parameter_set = params,
    nTrialsPerCondition = nTrialsPerCondition
  )
  
  # Structure tests
  expect_true(is.matrix(data))
  expect_equal(ncol(data), 4)
  expect_equal(colnames(data), c("sub", "cond", "rt", "accuracy"))
  expect_equal(nrow(data), nPart * nTrialsPerCondition * 2)  # *2 for two conditions
  expect_true(all(data[,"sub"] %in% 1:nPart))
  expect_true(all(data[,"cond"] %in% c(0,1)))
  
  # Value tests
  expect_true(all(data[,"rt"] > 0))
  expect_true(all(data[,"rt"] < 10))
  expect_true(all(data[,"accuracy"] %in% c(0, 1)))
  
  # Condition balance tests
  for(sub in 1:nPart) {
    sub_data <- data[data[,"sub"] == sub,]
    expect_equal(sum(sub_data[,"cond"] == 0), nTrialsPerCondition)
    expect_equal(sum(sub_data[,"cond"] == 1), nTrialsPerCondition)
  }
})

test_that("[Core] Data sampling - edge cases are handled correctly", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  source(CORE_FUNCTIONS$sample_data)
  source(CORE_FUNCTIONS$generate_dataset)
  source(CORE_FUNCTIONS$generate_trial)
  
  set.seed(456)  # Different seed to get positive drift rates
  
  # Use custom priors with larger drift rates and boundary separation
  priors <- data.frame(
    "bound_mean_mean"  = 2.50, "bound_mean_sdev"  = 0.20,  # Large boundary separation
    "drift_mean_mean"  = 2.00, "drift_mean_sdev"  = 0.50,  # Keep drift rate the same
    "nondt_mean_mean"  = 0.30, "nondt_mean_sdev"  = 0.06, 
    "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,   
    "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
    "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25,
    "betaweight_mean"  = 0.00, "betaweight_sdev"  = 1.00
  )
  
  params <- sample_parameters(
    priors = priors,
    nPart = 1,
    criterion = "drift",  # Changed from nondt
    modelType = "simple",
    Show = FALSE,
    X = c(0,1),  # X vector for 1 participant
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  # Single participant, single trial
  data1 <- sample_data(nPart = 1, nTrials = 1, parameter_set = params)
  expect_equal(nrow(data1), 1)
  expect_equal(ncol(data1), 3)
  
  # Single participant, single trial per condition
  data2 <- sample_data(nPart = 1, parameter_set = params, nTrialsPerCondition = 1)
  expect_equal(nrow(data2), 2)  # 1 participant * 1 trial * 2 conditions
  expect_equal(ncol(data2), 4)
})

test_that("[Core] Data sampling - data consistency", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  source(CORE_FUNCTIONS$sample_data)
  source(CORE_FUNCTIONS$generate_dataset)
  source(CORE_FUNCTIONS$generate_trial)
  
  set.seed(789)  # Different seed to get positive drift rates
  
  # Use custom priors with larger drift rates and boundary separation
  priors <- data.frame(
    "bound_mean_mean"  = 2.50, "bound_mean_sdev"  = 0.20,  # Large boundary separation
    "drift_mean_mean"  = 2.00, "drift_mean_sdev"  = 0.50,  # Keep drift rate the same
    "nondt_mean_mean"  = 0.30, "nondt_mean_sdev"  = 0.06, 
    "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,   
    "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
    "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25,
    "betaweight_mean"  = 0.00, "betaweight_sdev"  = 1.00
  )
  
  params <- sample_parameters(
    priors = priors,
    nPart = 3,
    criterion = "drift",  # Changed from nondt
    modelType = "simple",
    Show = FALSE,
    X = c(0,0,0,1,1,1),  # X vector for 3 participants
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  # Test participant numbering consistency
  data <- sample_data(nPart = 3, nTrials = 5, parameter_set = params)
  expect_setequal(unique(data[,"sub"]), 1:3)  # Participant numbers should be sequential
  
  # Test condition numbering consistency
  data_cond <- sample_data(nPart = 2, parameter_set = params, nTrialsPerCondition = 3)
  expect_setequal(unique(data_cond[,"cond"]), c(0,1))  # Condition values should be 0 and 1
  
  # Test trial count consistency
  for(sub in 1:2) {
    sub_data <- data_cond[data_cond[,"sub"] == sub,]
    expect_equal(sum(sub_data[,"cond"] == 0), 3)  # Each participant should have correct number of condition 0 trials
    expect_equal(sum(sub_data[,"cond"] == 1), 3)  # Each participant should have correct number of condition 1 trials
  }
})

test_that("[Core] Parameter sampler - drift rate generation", {
  source(CORE_FUNCTIONS$default_priors)
  source(CORE_FUNCTIONS$sample_parameters)
  
  # Custom priors with clear values for testing
  priors <- data.frame(
    "bound_mean_mean"  = 2.00, "bound_mean_sdev"  = 0.20,
    "drift_mean_mean"  = 2.00, "drift_mean_sdev"  = 0.50,
    "nondt_mean_mean"  = 0.30, "nondt_mean_sdev"  = 0.06,
    "bound_sdev_lower" = 0.10, "bound_sdev_upper" = 0.40,
    "drift_sdev_lower" = 0.20, "drift_sdev_upper" = 0.40,
    "nondt_sdev_lower" = 0.05, "nondt_sdev_upper" = 0.25,
    "betaweight_mean"  = 0.00, "betaweight_sdev"  = 1.00
  )
  
  # Test 1: Single participant with fixed beta
  set.seed(123)
  params1 <- sample_parameters(
    priors = priors,
    nPart = 1,
    criterion = "drift",  # Changed from nondt
    modelType = "simple",
    Show = FALSE,
    X = c(0,1),  # Minimal X vector for 1 participant
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  expect_length(params1$bound, 1)
  expect_length(params1$nondt, 1)
  expect_length(params1$drift, 2)  # Should have 2 drift rates for 1 participant
  
  # Test 2: Multiple participants with fixed beta
  set.seed(456)
  params2 <- sample_parameters(
    priors = priors,
    nPart = 3,
    criterion = "drift",
    modelType = "simple",
    Show = FALSE,
    X = c(0,0,0,1,1,1),  # X vector for 3 participants
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  expect_length(params2$bound, 3)
  expect_length(params2$nondt, 3)
  expect_length(params2$drift, 6)  # Should have 6 drift rates for 3 participants
  
  # Test 3: Check drift rate values with fixed beta
  set.seed(789)
  params3 <- sample_parameters(
    priors = priors,
    nPart = 2,
    criterion = "drift",
    modelType = "simple",
    Show = FALSE,
    X = c(0,0,1,1),  # X vector for 2 participants
    fixedBeta = 0.5  # Add fixedBeta to trigger drift doubling
  )
  
  expect_length(params3$drift, 4)  # Should have 4 drift rates for 2 participants
  expect_true(all(!is.na(params3$drift)))  # No NA values
  expect_true(all(is.finite(params3$drift)))  # All finite values
}) 