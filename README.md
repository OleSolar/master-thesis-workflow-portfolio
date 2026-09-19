# How Does Tourism Affect Housing Affordability Across Austria?

## Master's Thesis Portfolio

This repository presents my master's thesis on tourism intensity and housing affordability across Austrian political districts from 2015 to 2024. It highlights the data analysis, econometrics, data preparation and research skills used throughout the project.

The focus is on the full analytical workflow: turning a policy question into an empirical research design, preparing public data, constructing variables, selecting and validating an econometric model, testing robustness and communicating the results. The thesis received a grade of **1 (Very Good)**.

## Project Files

### Presentation and thesis

- [Portfolio presentation](Master_Thesis_Workflow.pdf) — research question, data engineering, model choices, robustness checks and main findings.
- [Full thesis](Master_Thesis_Solar_Public.pdf) — literature review, methodological details and complete results.

### R scripts, in workflow order

1. [Data cleaning](code/Data_cleaning.R) — reconciles source formats, STATcube publication symbols, district identifiers and reporting breaks.
2. [Variable creation](code/Variable%20creation%2C%20full%20dataset.R) — constructs tourism intensity, affordability, housing, demographic and labour-market variables.
3. [Preferred dataset construction](code/Creation%20of%20preferred%20dataset.R) — assembles the preferred district-year panel, including the Vienna aggregation.
4. [Final estimates](code/Final%20estimates.R) — static and dynamic benchmarks, panel diagnostics and an R System GMM cross-check of the preferred Stata specification.

### Prepared dataset

- [District-year analysis panel](data/processed/fullpanel_agg.csv) — the prepared dataset used in the estimation workflow, not the original raw extracts.
The 'Final Estimates.R' can be re-run in R using this data set and loading the packages

## Analytical Workflow

1. **Research design:** Define housing affordability as the outcome and tourism intensity as the main explanatory variable based on the economic mechanisms discussed in the thesis.
2. **Data preparation:** Import and clean public data from Statistik Austria and other official sources, remove publication symbols, standardise formats and reconcile changes in district definitions.
3. **Variable engineering:** Construct the affordability index, tourism intensity and economically motivated control variables, and assemble a consistent district-year panel.
4. **Econometric analysis:** Estimate static and dynamic benchmarks in R, then the preferred System GMM model in Stata, with an R cross-check. The model addresses persistence and potential endogeneity using internal instruments, subject to identifying assumptions.
5. **Validation and robustness:** Test for serial correlation and instrument validity, limit instrument proliferation, and compare alternative specifications and subsamples.
6. **Communication:** Present the workflow, model choices and results through concise tables, maps, code examples and plain-language interpretation.

## Main Result

The preferred model indicates that a 10% increase in tourism intensity is associated with about a **1.0% decline in housing affordability in the following year**. The relationship is stronger in highly tourism-dependent districts and clearer for house prices than for flats.

The presentation reports a preferred estimation sample of **822 observations across 93 districts**, with **60 instruments**, an **AR(2) p-value of 0.851** and a **Hansen p-value of 0.112**. These diagnostics support model assessment but do not prove the identifying assumptions.

The result is interpreted as an estimated association rather than definitive causal evidence. A key policy implication is that tourism and housing pressures should be considered jointly in districts where housing supply is constrained.

## Method Summary

- **Main data sources:** Statistik Austria / STATcube and the Austrian National Bank (OeNB).
- **Unit of analysis:** Austrian political districts observed annually from 2015 to 2024.
- **Outcome:** Net income per resident divided by the median house transaction price per square metre; higher values indicate greater affordability.
- **Main exposure:** Registered overnight stays per resident.
- **Controls:** Population density, housing stock, new dwellings, unemployment and migration, selected based on economic reasoning.
- **Preferred estimator:** Two-step Blundell–Bond System GMM with Windmeijer-corrected standard errors, collapsed instruments and bounded lag windows. The final thesis estimates were produced with Stata's `xtabond2`; R's `plm::pgmm` was used as a cross-check.
- **Model checks:** AR(2), Hansen and Difference-in-Hansen tests, together with alternative samples and specifications.
- **Exploratory extension:** A snow-based instrumental-variable design demonstrates geospatial data matching and an additional identification strategy, but is not treated as the main result.

## Using the R Workflow

The scripts are the original research scripts, organised in the order shown under **Project Files**. They document the analysis but are not a self-contained, one-command pipeline.

### Review the estimation using the prepared panel

Open R or RStudio with the repository root as the working directory, install the packages loaded in the estimation script, and import the panel:

```r
fullpanel_agg <- readr::read_csv(
  "data/processed/fullpanel_agg.csv",
  show_col_types = FALSE
)
```

Then work through the estimation sections in [Final estimates.R](code/Final%20estimates.R). Its later long-run tourism chart also requires the separate `tourism_nights_from_1974` object, which is not supplied by this panel.

### Rebuild from source data

Follow the four scripts in workflow order, adjusting the original local paths and importing the source objects they reference. The cleaning script contains an original `setwd()` path and assumes several objects have already been loaded; some export statements also depend on objects created elsewhere in the research workflow. Running the scripts unchanged from a clean R session is therefore not sufficient.

The original raw extracts, Stata do-files/logs and separate snow-instrument inputs are not currently included in this repository. The presentation describes the wider thesis workflow, including those components; the uploaded code and panel cover the R portion. The PDFs document the preferred Stata results and the exploratory snow-based design, rather than providing all materials needed to reproduce them.

## Skills Demonstrated

- Data cleaning using regular expressions, type conversion and missing-value handling.
- Multi-source joins, panel-data construction and feature engineering in R.
- Econometric modelling using fixed effects, dynamic panels and System GMM.
- Model diagnostics, robustness testing and careful interpretation of identification assumptions.
- Clear communication through maps, tables, code examples and a concise portfolio presentation.

Key R packages include `dplyr`, `tidyr`, `stringr`, `readr`, `fixest`, `plm` and `lmtest`. The presentation also illustrates geospatial matching using sf. This code example was not included in the final thesis and is therefore labelled as a reconstructed workflow.

## Interpretation Limits

The preferred System GMM model relies on internal instruments and identifying assumptions that cannot be fully verified through statistical tests. The exploratory snow-based design assesses instrument relevance, but its exclusion restriction also requires economic justification. The ten-year panel includes the COVID-19 disruption, and district-level aggregation may conceal local differences. Municipality-level data could not be accessed for this study because of privacy restrictions. Rental and property-level data were also unavailable from the public sources used.
