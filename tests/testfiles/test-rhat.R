library(testthat)
library(here)
source(here("tests", "helper-paths.R"))


test_that("[Rhat] Well-converged chains give Rhat close to 1", {
  source(CORE_FUNCTIONS$rhat)
  
  # Create chains from same distribution with large sample size
  set.seed(123)
  n_samples <- 10000
  chain1 <- rnorm(n_samples, mean = 0, sd = 1)
  chain2 <- rnorm(n_samples, mean = 0, sd = 1)
  chains <- cbind(chain1, chain2)
  rhat <- getRhat(chains)
  expect_lt(rhat, 1.01) # Well-converged chains should have Rhat very close to 1
})

test_that("[Rhat] Non-converged chains give high Rhat", {
  source(CORE_FUNCTIONS$rhat)
  
  # Create chains with different means
  set.seed(123)
  n_samples <- 100
  chain1 <- rnorm(n_samples, mean = 0, sd = 1)
  chain2 <- rnorm(n_samples, mean = 5, sd = 1) # Significantly different mean
  chains <- cbind(chain1, chain2)
  rhat <- getRhat(chains)
  expect_gt(rhat, 1.5) # Poor convergence should give high Rhat
})

test_that("[Rhat] Vector input is handled correctly", {
  source(CORE_FUNCTIONS$rhat)
  
  # Test vector with well-converged chains
  set.seed(123)
  chain1 <- rnorm(1000, 0, 1)
  chain2 <- rnorm(1000, 0, 1)
  vec <- c(chain1, chain2)
  rhat <- getRhat(vec, n.chains = 2)
  expect_lt(rhat, 1.1)
})

test_that("[Rhat] Input validation works correctly", {
  source(CORE_FUNCTIONS$rhat)
  
  # # Test vector input without n.chains
  # expect_error(getRhat(1:10), "Please specify the number of chains when input is a vector")
  
  # # Test vector with invalid length for n.chains
  # expect_error(getRhat(1:10, n.chains = 3), "Length of vector must be divisible by number of chains")
  
  # Test vector with valid length for n.chains
  expect_error(getRhat(1:9, n.chains = 3), NA)
})

test_that("[Rhat] Edge cases are handled correctly", {
  source(CORE_FUNCTIONS$rhat)
  
  # Test with small number of samples
  chain1 <- c(1, 2, 3)
  chain2 <- c(1.1, 2.1, 3.1)
  chains <- cbind(chain1, chain2)
  rhat <- getRhat(chains)
  expect_type(rhat, "double")
  expect_false(is.na(rhat))
  
  # Test with more than 2 chains
  set.seed(123)
  chains <- matrix(rnorm(3000), ncol = 3)
  rhat <- getRhat(chains)
  expect_lt(rhat, 1.1)
})
