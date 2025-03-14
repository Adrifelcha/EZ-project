# Hierarchical Bayesian EZ-DDM!

Welcome,

This repository contains all code needed to replicate the results reported 
throughout the various simulation studies and appendices in Chávez De la Peña
and Vandekerckhove (2024).

- `/code/` This folder contains all the R scripts and functions needed to
conduct a full simulation study. It contains two sub-folders:

    - `/code/functions/` : R scripts containing specific custom functions.

    - `/code/scripts/` : R scripts containing higher-level functions that call
    multiple specific custom functions to execute a full routine.

- `/examples/` This folder contains the code needed to replicate the analysis 
and figures presented in Appendices B and C.

    - `/examples/shape_perception/` : Materials related to the applied example
    in Appendix B.

    - `/examples/brightness_perception/` : Materials related to the applied 
    example in Appendix C.

- `/simulations/` This folder contains many sub-folders to store the specific
functions and scripts, as well as the output for different simulation studies.

    - `/simulations/generative_priors` : Six simulation studies where we use
    generative priors to sample true hierarchical parameters.

    - `/simulations/params_from_uniforms` : Six simulation studies where true
    hierarchical parameters are sampled from generative uniform distributions.

    - `/simulations/within-subject_ttest` : 

- `/figures/` This folder contains all figures included in the paper


# Project reproducibility

## Meta-regression example

**Folder:** `./examples/brightness_perception/` 

## Short hypothesis testing example

**Folder:** `./examples/shape_perception/`

## Small simulation study - Within-subject t-test design

**Folder:** `./simulations/within-subject_ttest/`

## Main simulation study 1 - Testing parameter recovery

**Folder:** `./simulations/params_from_uniforms/`

## Main simulation study 2 - Testing parameter recovery

**Folder:** `./simulations/generative_priors/`
