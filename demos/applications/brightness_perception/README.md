# Brightness Perception Demo Application

This example showcases the advantages of implementing a proxy model with a meta-regression extension that involves a non-linear function. We analyze data from [Ratcliff and Rouder (1998)](https://journals.sagepub.com/doi/pdf/10.1111/1467-9280.00067), experiment 1, participant N.H.

## Experimental Design

> ''...subjects were asked to decide **whether the overall brightness of pixel arrays displayed on a computer monitor was "high" or "low"** (Fig. 3a). The brightness of a display was controlled by the proportion of the pixels that were white. For each trial, the proportion of white pixels was chosen from one of two distributions, a high distribution or a low distribution, each with fixed mean and standard deviation (Fig. 3b). Feedback was given after each trial to tell the subject whether his or her decision had correctly indicated the distribution...''

The experiment has 66 conditions with two main factors:

- **Accuracy vs Speed instructions**: 
  - Conditions 1-33: Accuracy instruction (participants were told to prioritize making correct responses)
  - Conditions 34-66: Speed instruction (participants were told to prioritize responding quickly)

- **More black vs More white pixels**: 
  - Conditions 1-16 and 34-49: More black pixels (correct response is "low" brightness)
  - Conditions 18-33 and 51-66: More white pixels (correct response is "high" brightness)
  - Conditions 17 and 50: Ambiguous (50/50 black and white pixels) - these are excluded from analysis as they can't provide accuracy measures

## Model Description

The model has two components:

1. The main component is the proxy model built from the sampling distributions for the summary statistics used by the EZ-DDM.

2. The second component is a meta-regression structure with which we explore the effects of instruction and stimulus configuration on the boundary separation and drift rate.

## Results

Our main focus is on the effect of instruction on the drift rate. The results suggest that:

1. Drift rates decrease in both instruction conditions as the stimulus configuration approaches the 50/50 condition, where the task is more difficult (as expected)
2. Drift rates are shifted upwards in the 'Speed' instruction condition, indicating an effect of instruction condition on drift rate (The instruction condition had an effect on the drift rate parameter!)

The model provides an adequate account of the data analyzed, as demonstrated by posterior predictive checks for all EZ summary statistics.

## Performance

This analysis with 7,802 observations after data cleaning took 8.6 seconds on an off-the-shelf desktop computer.

## Files in this Directory

- `data/`: Contains the raw experimental data
- `src/`: Contains R scripts specific to this demo application
  - `run_metaregression-example.R`: Runs the entire demo and stores results in an output file
  -  `plot_...R`: Replicate the figures used in the paper (and other conference and poster presentations)

## Main report

See the `brightness-demo-report.html` file for a detailed walkthrough the code, model specification in JAGS, and results.