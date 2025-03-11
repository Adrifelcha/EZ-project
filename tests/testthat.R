library(testthat)
library(here)
library(crayon)

# Source helper paths first
source(here("tests", "helper-paths.R"))

# Custom test reporter to show test names and collect results
SummaryReporter <- R6::R6Class("SummaryReporter",
  inherit = Reporter,
  public = list(
    results = NULL,
    current_context = NULL,
    current_test = NULL,
    
    initialize = function() {
      super$initialize()
      self$results = list()
    },
    
    start_context = function(context) {
      self$current_context <- context
    },
    
    start_test = function(context, test) {
      self$current_test <- test
      cat(silver("Running: "), test, "\t")
    },
    
    add_result = function(context, test, result) {
      if (!test %in% names(self$results)) {
        self$results[[test]] <- list(
          passes = 0,
          failures = 0,
          errors = 0,
          skips = 0,
          warnings = 0
        )
      }
      
      if (inherits(result, "expectation_success")) {
        self$results[[test]]$passes <- self$results[[test]]$passes + 1
      } else if (inherits(result, "expectation_failure")) {
        self$results[[test]]$failures <- self$results[[test]]$failures + 1
      } else if (inherits(result, "expectation_error")) {
        self$results[[test]]$errors <- self$results[[test]]$errors + 1
      } else if (inherits(result, "expectation_skip")) {
        self$results[[test]]$skips <- self$results[[test]]$skips + 1
      }
      
      if (!is.null(result$warning)) {
        self$results[[test]]$warnings <- self$results[[test]]$warnings + 1
      }
    },
    
    end_test = function(context, test) {
      result <- self$results[[test]]
      status <- if (result$failures > 0 || result$errors > 0) {
        red(bold("✖ FAIL"))
      } else if (result$warnings > 0) {
        yellow(bold("⚠ WARN"))
      } else if (result$skips > 0) {
        yellow(bold("⚡ SKIP"))
      } else {
        green(bold("✓ PASS"))
      }
      cat(status, "\n")
    },
    
    end_reporter = function() {
      # Print summary table
      cat("\n", bold(underline("Test Summary")), "\n\n")
      
      # Calculate maximum test name length for padding
      max_name_length <- max(nchar(names(self$results)))
      
      # Print header
      header <- sprintf(
        "%-*s |%6s |%6s |%6s |%6s |%6s |",
        max_name_length,
        "Test",
        " Pass",
        " Fail",
        "Error",
        " Skip",
        " Warn"
      )
      cat(bold(header), "\n")
      
      # Print separator
      sep <- paste(rep("-", nchar(header)), collapse="")
      cat(sep, "\n")
      
      # Little helper function to format numbers in exactly 7 chars...
      format_num <- function(n, type) {
        num <- style_number(n, type)
        if (nchar(as.character(n)) == 1) {
          sprintf("    %s ", num)
        } else if (nchar(as.character(n)) == 2) {
          sprintf("   %s ", num)
        } else {
          sprintf("  %s ", num)
        }
      }
      
      # Print results for each test
      for (test in names(self$results)) {
        r <- self$results[[test]]
        status <- sprintf(
          "%-*s |%s |%s |%s |%s |%s |",
          max_name_length,
          test,
          format_num(r$passes, "pass"),
          format_num(r$failures, "fail"),
          format_num(r$errors, "error"),
          format_num(r$skips, "skip"),
          format_num(r$warnings, "warn")
        )
        cat(status, "\n")
      }
      
      # Print separator
      cat(sep, "\n")
      
      # Print totals
      totals <- list(
        passes = sum(sapply(self$results, function(x) x$passes)),
        failures = sum(sapply(self$results, function(x) x$failures)),
        errors = sum(sapply(self$results, function(x) x$errors)),
        skips = sum(sapply(self$results, function(x) x$skips)),
        warnings = sum(sapply(self$results, function(x) x$warnings))
      )
      
      total_line <- sprintf(
        "%-*s |%4s |%4s |%4s |%4s |%4s |",
        max_name_length + 9,
        bold("Total"),
        format_num(totals$passes, "pass"),
        format_num(totals$failures, "fail"),
        format_num(totals$errors, "error"),
        format_num(totals$skips, "skip"),
        format_num(totals$warnings, "warn")
      )
      cat(bold(total_line), "\n\n")
      
      # Print overall status
      if (totals$failures > 0 || totals$errors > 0) {
        cat(red(bold("✖ Some tests failed\n")))
      } else if (totals$warnings > 0) {
        cat(yellow(bold("⚠ All tests passed with warnings\n")))
      } else if (totals$skips > 0) {
        cat(yellow(bold("⚡ All tests passed, some skipped\n")))
      } else {
        cat(green(bold("✓ All tests passed successfully\n")))
      }
    }
  )
)

# Helper function to style numbers
style_number <- function(n, type) {
  if (n == 0) return(silver("0"))
  
  style <- switch(type,
    "pass" = green,
    "fail" = red,
    "error" = red,
    "skip" = yellow,
    "warn" = yellow,
    identity
  )
  
  style(as.character(n))
}

# Run all tests in the testfiles directory with custom reporter
test_dir(
  path = here("tests", "testfiles"),
  reporter = SummaryReporter
)

