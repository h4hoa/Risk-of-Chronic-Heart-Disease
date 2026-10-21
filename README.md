# Risk of Chronic Heart Disease

## Overview

This project investigates the relationship between the Framingham Risk Score and selected health-related explanatory variables using statistical modelling.

Completed as a collaborative final assessment for the Applied Linear Models course at the University of Waterloo, the project applies logistic regression and cross-validation to examine relationships in the data.

## Research Question

How are selected explanatory variables associated with the Framingham Risk Score in the dataset?

## Methodology

- Explored the provided Framingham Heart Study dataset.
- Examined relationships between the outcome of interest and explanatory variables.
- Applied logistic regression models.
- Used cross-validation to assess model performance.

## Results

The response was the logit of the CHD risk score, log(p / (1 − p)), modelled with linear regression on data from 2,306 participants in the Framingham Heart Study.

**Candidate models**
- **Model 1 (automated selection):** stepwise selection starting from the main-effects model, with the full two-way interaction model as the upper bound. The selected model has 58 terms (17 main effects plus interactions), adjusted R² = 0.841, residual SE = 0.476.
- **Model 2 (interpretable):** all main effects except total cholesterol and LDL cholesterol, which were removed because of their high variance inflation factors. It has 15 predictors, adjusted R² = 0.772, residual SE = 0.570.

**Model selection:** In 2,000 replications of cross-validation (1,500 training / 806 test observations), Model 1 had lower root mean squared prediction error and a higher out-of-sample log-likelihood. It was chosen as the final model.

**Key factors:** The strongest main effects were age, sex (male), cigarettes per day, diabetes, heart rate, blood pressure medication, and a history of heart attack, stroke, or hypertension, all associated with higher risk. HDL cholesterol was associated with lower risk in both models.

**Limitations**
- Many main effects in Model 1 (including total cholesterol, LDL cholesterol, blood pressure, and glucose) have high p-values, but they were retained because they appear in significant interaction terms. This makes their individual effects hard to interpret.
- With 58 terms, Model 1 may overfit. Model 2 predicts less well but is much easier to interpret.
- Rows 276, 892, and 1141 were identified as high-influence observations that may distort the estimated relationships.

## Tools

- R
- R Markdown
- Logistic regression
- Cross-validation
- Statistical analysis and visualization

## Repository Contents

- `final_report.Rmd` — analysis and report source
- `final_report.pdf` — final project report
- `external_code.R` — supporting code
- `fhs.csv` — dataset file, subject to applicable data-sharing and licensing terms
- `time_consuming_calculation.pdf` — supporting calculation document

## Reproducibility

The analysis was developed in R 3.6.x (2020) and documented in R Markdown.

**Required packages:** `rmarkdown`, `knitr`, and `statmod`. All modelling functions (`lm`, `step`, `anova`, `cooks.distance`, etc.) come from base R. Because the report knits to PDF, a LaTeX distribution is also needed (for example, via `tinytex::install_tinytex()`).

​```r
install.packages(c("rmarkdown", "knitr", "statmod", "tinytex"))
​```

**Files needed (all in the repository root):**
- `final_report.Rmd`: the main report
- `external_code.R`: figure and appendix code, read into the report with `knitr::read_chunk()`
- `fhs.csv`: the Framingham Heart Study dataset

**To reproduce:** clone the repository, open `final_report.Rmd` in RStudio, and click Knit. Keep all three files in the same folder, since the data and external code are loaded with relative paths.

**Runtime:** knitting re-runs the automated model selection and the 2,000-replication cross-validation, so it takes several minutes. The cross-validation alone took about 4 minutes on the original machine. `time_consuming_calculation.pdf` contains the output of these slow steps for reference.

**Randomness:** The original document didn't include a fixed seed, but feel free to set a seed before cross-validation so results are identical across runs.

## Limitations

The analysis is based on the available dataset and modelling assumptions. Associations identified in this project should not be interpreted as proof of causation or as a clinically validated diagnostic tool.

