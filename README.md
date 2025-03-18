# Hierarchical Bayesian EZ Drift Diffusion Model!

Welcome,

This repository contains all code needed to replicate the results reported throughout all simulations and applied examples presented in our paper:

> Chávez De la Peña, A. F., & Vandekerckhove, J. (preprint). An EZ Bayesian hierarchical drift diffusion model for response time and accuracy.

This repository is organized as follows:

- `/src/` This folder contains the source code to support the simulation studies and plotting custom functions.

- `/demos/` This folder contains scripts (Rscripts and Rmarkdown executive reports) replicating the analysis and figures presented in each appendix in our paper. These include:

    - `/demos/applications/` : Materials related to the applied examples in Appendices B and C.

        - `/demos/applications/shape_perception/` : Application example on a shape perception task run by Vandekerckhove et al. (2007) to showcase the use of our proxy model for hypothesis testing, by building a multiple linear regression model extension.

        - `/demos/applications/brightness_perception/` : Application example on a brightness perception task run by Ratcliff et al. (1998) to showcase the use of metaregression extensions on our proxy model, including a non-linear function.

    - `/demos/simulations/` : Materials related to the simulation studies described in the paper

        - `/demos/simulations/hypothesis_testing/` : Simulation study with a within-subject t-test design on the drift rate parameter. We use this simulation study to explore our proxy model's performance for hypothesis testing.

        - `/demos/simulations/generative_uniforms/` : Simulation study where true parameter values used to generate data are sampled from uniform distributions.

        - `/demos/simulations/generative_priors/` : Simulation study where true parameter values used to generate data are sampled from the prior distributions.

- `/output/` This folder serves as a default output folder for all simulation results, figures and BUGS models.


# Project reproducibility

The repository includes a virtual machine image that can be used to replicate all results.

# Contact

For any questions or feedback, please contact me at:

> Adriana Felisa Chávez De la Peña: adrifelcha@gmail.com 