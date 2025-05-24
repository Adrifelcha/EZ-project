# Archive-revised: EZ Bayesian Hierarchical DDM 

This branch preserves the state of the repository at the moment of **submitting the revised version of our paper** to Psychonomic Bulletin & Review (on May, 2025). 

The revised submission incorporated:

- Minor changes to the manuscript's content (i.e., adding the JAGS code used with each appendix)
- Small fixes to the figures generated from the **applied examples** in the appendices.
- A major revision of the **hypothesis testing simulation study** reported in one of the appendices.

The current branch also includes some changes made to the repo organization and source code improvements that were incorporated after submission. Please continue reading if you're interested in replicating any particular result or figure reported on the paper exactly.

### Replicability

1. **Applied examples** . To replicate the applied examples we present in the paper's appendices, please refer to the 'demos/applications/' folder, where you will find executive .Rmd report and code to replicate the exact same figures included in the paper and derived presentations.

2. **Hypothesis-testing simulation study**. To replicate the hypothesis-testing simulation study reported in one of the appendices, please run script 'runParallelSeeds.R' stored in the 'demos/simulation-studies/hypothesis_testing/' directory. Replicating this simulation study in full should take around 2 hours, depending on your computer. Code to replicate the figures included in the paper can be found in the '/scripts/' subfolder.

3. **Parameter recovery simulation studies:** To replicate the exact same results reported for the main simulation study, please refer to the `archive-submitted` branch of the repo. 

    **Note:** The current branch includes changes made to the source code used to run parameter recovery simulation studies. Code to run full parameter recovery simulation studies is provided in the 'demos/simulation-studies/' folder. The results are comparable to what is reported in the paper, and they take less time to run. However, they won't reproduce the exact same figures included in the paper.

## Repository Branch Organization

- **archive-submitted**: Archive of the working repository as of the day of submission (November 29, 2024). Ensures the reproducibility of the figures included when reporting the main simulation-study results.
- **archive-revised (this branch)**: Archive of the working repository as of the day of revision submission (May 20, 2025). This branch incorporates some major changes to the hypothesis testing simulation study, and some minor revisions to the applied examples, that were included in the papers revision. This branch also includes some general changes and cleaning to the repo structure and organization.
- **Main branch**: Please refer to the 'main' branch for the latest and most refined version of this repository.