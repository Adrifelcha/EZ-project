# /RData-results/ 

This folder contains the results of the simulation studies and demo applications.

Files stored in this folder are organized as follows:

## Simulation study outputs

Files that start with `sim_` contain the results of a given simulation study

    - P-: Indicates the number of participants simulated.
    - T-: Indicates the number of trials per condition simulated.
    - D-: Indicates the number of conditions simulated.
    - Hier: Indicates that the generative model was a simple hierarchical model.
    - Meta: Indicates that the generative model incorporated a metaregression extension.
    - Ttst: Indicates that the generative model incorporated a t-test extension.
    - fromPrior: Indicates that true parameters were drawn from the prior distributions.
    - fromUnif: Indicates that true parameters were drawn from uniform distributions.

Files that start with `simHypTesting_` contain the results of the hypothesis testing simulation study, which uses a within-subject t-test design on the drift rate parameter.

## Demo application outputs

Files that start with `demo_` contain the results of the demo application.

    - demo_brightness_...: Results from fitting a Hierarchical Bayesian EZ-DDM with a metaregression extension on the drift rate and boundary separation parameters to the brightness perception dataset collected by Ratcliff and Rouder (1998).
    - demo_shape_...: Results from fitting a Hierarchical Bayesian EZ-DDM with a multiple linear regression extension on the drift rate parameter to data from a shape perception study run by Vandekerckhove et al. (2007).

## Other outputs


