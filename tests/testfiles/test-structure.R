library(testthat)
library(here)

rm(list = ls())

# Source helper paths first
source(here("tests", "helper-paths.R"))

test_that("[Structure] All required directories exist", {
  for (dir in DIRS) {
    expect_true(dir.exists(dir))
  }
})

test_that("[Structure] All core function files exist", {
  for(file in FUNCTION_FILES) {
    expect_true(file.exists(file.path(CODE_DIRS$functions, file)))
  }
})

test_that("[Structure] All required script files exist", {
  for(file in SCRIPT_FILES) {
    expect_true(file.exists(file.path(CODE_DIRS$scripts, file)))
  }
})

test_that("[Structure] All core functions can be sourced", {
  expect_error(source(CORE_FUNCTIONS$generate_trial), NA)
  expect_error(source(CORE_FUNCTIONS$generate_dataset), NA)
  expect_error(source(CORE_FUNCTIONS$default_priors), NA)
  expect_error(source(CORE_FUNCTIONS$default_inits), NA)
}) 