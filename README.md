# Recovac

## Overview

Recovac is an R-based data preparation and analysis bundle for studying COVID-19 outcomes in kidney patients, with a focus on dialysis and transplant populations.

The repository appears to build a linked analytical dataset from multiple CBS/healthcare sources and then use it for:
- cohort construction for kidney patients
- comorbidity extraction
- COVID test and admission linkage
- mortality and SMR-style analyses
- regression-ready data preparation
- descriptive summaries and quality checks

Most scripts rely on `data.table`, `arrow`, and project-specific helper functions loaded from `src/setup.R` and `src/functions.R`.

## Linked Datasets

The scripts reference several linked administrative and healthcare datasets. In broad terms, these datasets add the following information:

- **Demographics / stapeling**
  - Age, sex, SES/WOA category, and other population characteristics
  - Used as the base population frame for mortality and regression analyses

- **Nefrovisie kidney patient files**
  - Dialysis and transplant status over time
  - Monthly kidney treatment status
  - Used to define the kidney cohort and status switching

- **LBZ hospital admissions**
  - Hospital admission records and ICD-10 diagnoses
  - Used to identify COVID admissions and non-COVID/non-nephrology admissions

- **COVID test data**
  - Test results and infection dates
  - Used to derive positive test history and period-specific indicators

- **Comorbidity / aandoeningen data**
  - Chronic conditions such as diabetes, asthma/COPD, hypertension, cholesterol
  - Combined with medication data to derive immunosuppression and comorbidity flags

- **Medication data**
  - ATC4 medication codes
  - Used to identify immunosuppressive medication exposure

- **Mortality data**
  - Death dates and COVID death flags
  - Used for monthly mortality, SMR calculations, and regression outcomes

- **Healthcare cost datasets**
  - ZVW, Wlz, PGB, MSZ, and DBC-related cost sources
  - Used in the frailty/cost exploration script

## Repository Structure

- `iterartion_2/00_comorbidities.R`
  - Builds a person-year comorbidity file by combining aandoeningen and medication data
- `iterartion_2/00_covid_admissions.R`
  - Extracts COVID-19 hospital admissions from LBZ data
- `iterartion_2/00_covid_tests.R`
  - Extracts COVID test results and creates a person-week test dataset
- `iterartion_2/00_frailty.R`
  - Explores healthcare costs and related frailty/cost measures for kidney patients
- `iterartion_2/00_kidney_patients.R`
  - Builds the kidney patient monthly status file from Nefrovisie sources
- `iterartion_2/00_non_covid_nefro_admissions.R`
  - Extracts general hospital admissions excluding COVID and nephrology diagnoses
- `iterartion_2/01_sampling.R`
  - Computes sampling / SMR-related structures by month, year, and demographic strata
- `iterartion_2/02_prep_regression_data.R`
  - Assembles the regression dataset by merging population, kidney status, comorbidity, admission, test, and mortality data
- `iterartion_2/03_descriptives.R`
  - Produces descriptive statistics and Excel outputs for cohort summaries
- `iterartion_2/03_make_output_v2.R`
  - Combines model output tables into a final reporting dataset
- `iterartion_2/03_regressions.R`
  - Runs regression models across periods, outcomes, and covariate sets
- `iterartion_2/99_check_status_switching.R`
  - Quality check for kidney status switching over time

## Source Code Summary

### Data extraction and harmonization
- Standardizes raw source files with `format_data()`
- Reads CSV, Parquet, and SAV files from external CBS/healthcare locations
- Filters and deduplicates records at the person-week or person-year level

### Cohort construction
- Defines kidney patients from Nefrovisie dialysis/transplant data
- Separates dialysis and transplant status
- Removes or flags people with double therapy where needed

### Event derivation
- Creates indicators for:
  - COVID-positive tests
  - COVID admissions
  - all-cause and COVID mortality
  - non-COVID/non-nephrology admissions
  - comorbidity and immunosuppression exposure

### Analysis preparation
- Builds regression-ready person-level datasets
- Creates period indicators for COVID waves
- Adds demographic, SES, comorbidity, and admission covariates
- Prepares stratified SMR-style monthly data

### Reporting and checks
- Generates descriptive tables
- Combines model outputs into final result tables
- Includes sanity checks for treatment status changes over time

## Output Artifacts

No output files were provided in this bundle, and the `output_upload_name` is disabled.

However, the scripts are designed to write intermediate and final artifacts such as:

- `data/raw/comorbidities.parquet`
- `data/raw/opnames_2020_2021.parquet`
- `data/raw/covid_tests_2020_2021.parquet`
- `data/raw/non_covid_nefro_opnames.parquet`
- `data/raw/nefro_year_month_k1_k3.parquet`
- regression and SMR result tables in `data/output/...`
- Excel summary workbooks from descriptive scripts

## Next Steps

- Add a top-level `README` with run order and dependency notes
- Document required helper functions from `src/setup.R` and `src/functions.R`
- Replace hard-coded network paths with configurable project paths
- Add a reproducible pipeline script or `targets`/`drake` workflow
- Include a data dictionary for derived variables and output tables
- Add example input/output schemas for the main parquet files
- Clarify which scripts are intended for production runs versus exploratory checks
