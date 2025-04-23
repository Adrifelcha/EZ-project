# /src/

This folder contains the source code for the repository.

This folder is organized into the following subfolders:
    
    - /calculation/: custom functions to process data and model outputs in order to calculate relevant statistics.
    - /console-display/: custom functions to print relevant information and updates to the console.
    - /loading/: custom functions to load files (i.e., data, model outputs, custom functions, etc.)
    - /main-macroFuntions/: HIGHER-LEVEL scripts that call the functions in the other files to complete a step necessary for a simulation study:
        - HDDM_setup.R: This data generates parameter values and data for a simulation study.
        - HDDM_runJAGS.R: This data runs the JAGS model on the data generated in the previous step.
        - HDDM_runSims.R: This data runs a single simulation study (Fixed parameter set, fixed number of participants, fixed number of trials).
        - HDDM_runFullSeed.R: This data runs a full simulation study across different numbers of participants and trials, generating new parameter values and data for each iteration.    
    - /plotting/: custom plotting functions.            
    - /run-JAGS/: custom functions to set-up and run the JAGS model.    
    - /simulation-samples/: sample parameter values and data for simulation study.
    - /storing/: custom functions to save data and model outputs.
    