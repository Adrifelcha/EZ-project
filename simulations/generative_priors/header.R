for(archive in dir(here::here("code", "functions"))) {
  source(here::here("code", "functions", archive))
}

for(archive in dir(here::here("simulations", "generative_priors", "functions"))) {
  source(here::here("simulations", "generative_priors", "functions", archive))
}

source(here::here("code", "scripts", "HDDM_setup.R"))
source(here::here("code", "scripts", "HDDM_runJAGS.R"))
