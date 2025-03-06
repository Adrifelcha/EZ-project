library(here)

# Define base directories
BASE_DIRS <- list(
  code = here("code"),
  examples = here("examples"),
  results = here("results"),
  tests = here("tests")
)

# Define subdirectories of src
CODE_DIRS <- list(
  functions = file.path(BASE_DIRS$code, "functions"),
  scripts = file.path(BASE_DIRS$code, "scripts")
)

# Define subdirectories of tests
TEMP_DIRS <- list(
  tmp = file.path(tempdir())    # random temp dir
)

# Define output files
OUTPUT_FILES <- list(
  jags_model = file.path(TEMP_DIRS$tmp, "model.bug")
)

# Combine all directories for testing
DIRS <- c(unlist(BASE_DIRS), unlist(CODE_DIRS))

# Define expected function files
FUNCTION_FILES <- c(
  "write_JAGSmodel.R",
  "show_priors.R", 
  "rhat.R",
  "sample_data.R",
  "sample_parameters.R",
  "generate_dataset.R",
  "generate_trial.R",
  "default_priors.R",
  "default_inits.R",
  "data_toJAGS.R",
  "namingFunctions.R"
)

# Define expected script files
SCRIPT_FILES <- c(
  "HDDM_setup.R",
  "HDDM_runJAGS.R",
  "HDDM_runSims.R",
  "HDDM_runFullSeed.R"
)

# Define core function paths for frequent use
CORE_FUNCTIONS <- list(
  generate_trial = file.path(CODE_DIRS$functions, "generate_trial.R"),
  generate_dataset = file.path(CODE_DIRS$functions, "generate_dataset.R"),
  default_priors = file.path(CODE_DIRS$functions, "default_priors.R"),
  default_inits = file.path(CODE_DIRS$functions, "default_inits.R"),
  data_toJAGS = file.path(CODE_DIRS$functions, "data_toJAGS.R"),
  rhat = file.path(CODE_DIRS$functions, "rhat.R"),
  write_JAGSmodel = file.path(CODE_DIRS$functions, "write_JAGSmodel.R"),
  sample_parameters = file.path(CODE_DIRS$functions, "sample_parameters.R"),
  sample_data = file.path(CODE_DIRS$functions, "sample_data.R"),
  naming_functions = file.path(CODE_DIRS$functions, "namingFunctions.R")
) 