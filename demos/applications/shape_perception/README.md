# Shape Perception Demo Application
## Hypothesis Testing Example

This example showcases the implementation of our proxy model with a meta-regression extension that uses a multiple linear regression function for hypothesis testing. We re-analyze data from [Vandekerckhove, Panis and Wagemans (2007)](https://link.springer.com/article/10.3758/BF03193960), who studied nine participants in a shape perception experiment.

## Experimental Design

Participants were asked to compare pairs of irregular shapes in a "Same/Different" task. The experimental design included three factors:

1. **Change presence**: Whether there was a change between the shapes
2. **Change type**: If there was a change, whether it affected a *convexity* or *concavity*
3. **Change quality**: If there was a change, whether it introduced a *qualitatively* new vertex or *quantitatively* changed an existing one

The incompletely crossed design leads to five experimental conditions:

| Condition | Change (A) | Change quality (B) | Change type (C) |
|-----------|------------|-------------------|----------------|
| k = 1     | Yes (A = 1) | Qualitative (B = 0) | Convexity (C = 0) |
| k = 2     | Yes (A = 1) | Quantitative (B = 1) | Convexity (C = 0) |
| k = 3     | Yes (A = 1) | Qualitative (B = 0) | Concavity (C = 1) |
| k = 4     | Yes (A = 1) | Quantitative (B = 1) | Concavity (C = 1) |
| k = 5     | No (A = 0)  | n/a | n/a |

## Model Description

We explore the variability in the drift rate parameter across conditions using a multiple linear regression model with regression coefficients for the effects of change presence, change quality, and change type, as captured by the dummy variables A, B, and C.

The predicted drift rate for each condition is determined by its unique configuration of the dummy variables A, B, and C. The model parameters have the following interpretations:

| Condition | Drift predicted | Interpretation of parameter added |
|-----------|----------------|-----------------------------------|
| 1 | μ | μ is the baseline drift rate |
| 2 | μ + γ₁ | γ₁ is the effect of a quantitative change |
| 3 | μ + γ₂ | γ₂ is the effect of a change in concavity |
| 4 | μ + γ₁ + γ₂ + γ₃ | γ₃ is an interaction term |
| 5 | μ + γ₄ | γ₄ is the effect of not having any change |

## Results

Our analysis focuses on the regression effects γ₁, γ₂, and γ₃:

1. For γ₁ (effect of change quality)
2. For γ₂ (effect of change type)
3. For γ₃ (interaction effect)

The posterior distributions of the predicted drift rates show a clear effect of change type (concavity vs. convexity), while other effects are less pronounced.

## Performance

This analysis with 5,722 observations after data cleaning took 3.8 seconds on an off-the-shelf desktop computer.

## Files in this Directory

- `data/`: Contains the raw experimental data
- `src/`: Contains R scripts specific to this demo application
  - `run_hypoTesting-example.R`: Runs the entire demo and stores results in an output file
  - `plot_...R`: Replicate the figures used in the paper

## Main report

See the `shape-demo-report.html` file for a detailed walkthrough of the code, model specification in JAGS, and results.
