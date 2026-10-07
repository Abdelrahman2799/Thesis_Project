# Prenatal Exposure to Maternal Distress and Adult Stress Physiology

**MSc thesis · Statistics and Data Science (Data Science specialization) · Hasselt University, 2025–2026**
Supervisors: dr. Cécile Kremer (Hasselt University) and Prof. Dr. Bea Van den Bergh (KU Leuven).

📄 **[Read the full thesis (PDF)](My_Master_Thesis.pdf)**

## Question

Does anxiety during pregnancy have long-term effects on how the child's body reacts to stress as an adult?

I followed up adults (27–29 years old) from a longitudinal birth cohort and compared those whose mothers had **high vs low anxiety during pregnancy**. I looked at their stress responses during a standardized laboratory stress protocol, measured through several physiological systems.

## Data

- **51 participants** from a prospective cohort that started during pregnancy, split into low and high prenatal maternal anxiety groups (75th percentile cut-off on maternal state anxiety)
- **Lab stress protocol** with rest and stress segments, ending with a socially evaluated **cold pressor task** (hand in ice water while being watched), followed by recovery
- **Outcomes**, repeated over time for each person:
  - **Heart rate variability (HRV):** RMSSD, SDNN, RSA, HF, LF, LF/HF
  - **Pre-ejection period (PEP)** and **skin conductance level (SCL)**
  - **Saliva samples:** cortisol, alpha-amylase, and their ratio, taken at up to 8 time points

The data come from a confidential cohort study and are **not included** in this repository.

## Methods

| Step | What I did |
|---|---|
| Preprocessing | Log-transformed skewed outcomes, quality-coded unreliable recordings as missing, aligned saliva samples on *time since the stress task* |
| Exploration | Individual profiles, mean and variance profiles by group, sex, and condition; semi-variograms to check serial correlation |
| Modeling | **Linear mixed-effects models** with a random intercept per participant and an **AR(1)** residual structure where supported; baseline, reactivity, and recovery effects; group × sex × time interactions |
| Missing data (MAR) | **Direct likelihood** and **multiple imputation** (10 imputations) under missing at random |
| Missing data (MNAR) | **Pattern-mixture sensitivity analysis with δ-adjustment**: three missing-not-at-random scenarios (missing more under stress, in the high-anxiety group, or both), with δ from −0.3 to +0.3 |
| Summary metrics | Change from baseline, recovery slopes, and peak responses |

## Main findings

- For most outcomes, including the HRV indices and cortisol, there was **no evidence** that prenatal maternal anxiety changes stress **reactivity or recovery** in adulthood.
- Some HRV measures showed differences in **resting (tonic) autonomic activity**, and alpha-amylase showed **sex-specific** associations at baseline and peak levels.
- These effects did not show up as different stress trajectories over time, and were not consistent across physiological systems.
- The conclusions held up under the **MNAR sensitivity analyses**.

**Conclusion:** prenatal maternal anxiety does not seem to cause widespread or lasting problems in adult stress regulation, but it may be linked to small, context-dependent differences in baseline autonomic function.

## Repository structure

```
├── My_Master_Thesis.pdf             # Full thesis
├── hrv_exploration.Rmd              # Exploratory analysis of HRV/SCL data (R)
├── HRV_DL.sas                       # Direct-likelihood mixed models, HRV/SCL (SAS)
├── HRV_MI_MAR_MNAR.sas              # Multiple imputation (MAR) + δ-adjusted MNAR analysis (SAS)
├── alpha-amylase.sas                # Cortisol, alpha-amylase, and ratio models (SAS)
├── HRV_exploratory_data_analysis/   # Individual, mean, and variance profile plots
├── HRV_model_diagnostics/           # Residual, QQ, and linearity plots (HRV/SCL)
├── AAS_model_daigostics/            # Residual, QQ, and linearity plots (saliva biomarkers)
├── HRV_MNAR_Results/                # Pooled MNAR sensitivity results per HRV outcome
└── AAS_CPT_MNAR_Results/            # MNAR sensitivity results for saliva outcomes
```

File paths in the scripts are left out on purpose, since the data are not public.

## Tools

`SAS` (PROC MIXED, PROC MI, PROC MIANALYZE, PROC VARIOGRAM) · `R` (R Markdown, tidyverse, ggplot2)

## Skills shown

Longitudinal data analysis · linear mixed-effects models · residual covariance selection · multiple imputation · MNAR sensitivity analysis · model diagnostics · reproducible reporting
