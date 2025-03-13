# /src/

This folder contains the source code for the repository.

This folder is organized as follows:
    
    - get_...: files containing custom functions to process data and model outputs.
    - JAGS_...: files containing custom functions to interact with JAGS.
    - plot_...: files containing custom plotting functions.        
    - sample_...: files containing custom functions to sample parameter values and data.    
    - show_...: files containing custom functions to print relevant information to the console.
    - store_...: files containing custom functions to save data and model outputs.

The folder also contains a series of HIGHER-LEVEL scripts that call the functions in the other files to complete a step necessary for a simulation study:

    - HDDM_setup.R: This data generates parameter values and data for a simulation study.
    - HDDM_runJAGS.R: This data runs the JAGS model on the data generated in the previous step.
    - HDDM_runSims.R: This data runs a single simulation study (Fixed parameter set, fixed number of participants, fixed number of trials).
    - HDDM_runFullSeed.R: This data runs a full simulation study across different numbers of participants and trials, generating new parameter values and data for each iteration.