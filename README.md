# Edinburgh Cycling Demand Modelling

A portfolio-friendly version of a university statistical computing group project. The project models daily cycling demand in Edinburgh from 2020 to 2025 using weather, seasonality, day-of-week effects and long-term trend variables.

## Project objective

The aim is to build and evaluate a sequence of linear regression models for daily cyclist counts. The analysis focuses on three practical planning questions:

1. How reliably can demand be predicted across different seasons?
2. How sensitive is cycling demand to temperature?
3. What does the estimated long-term trend imply for transport planning?

## Repository structure

```text
edinburgh-cycling-demand/
├── README.md
├── Report_project02.Rmd
├── Report_project02_portfolio.pdf
├── code.R
├── cycle_daily_df.Rdata
├── docs/
│   ├── assignment_summary.md
│   ├── interview_talking_points.md
│   └── academic_integrity_note.md
└── .gitignore
```

This keeps the original coursework-style structure: the R Markdown report, analysis script and data file are in the same folder so that the report can be knitted directly.

## Data

The data file `cycle_daily_df.Rdata` contains one data frame called `cycle_daily_df`, with approximately 2,180 daily observations. Main variables include:

- `date`: date of observation
- `count`: total daily cyclist count across active stations
- `dow`: day of week
- `month`, `year`, `weekend`: calendar/time indicators
- `temp_mean`, `temp_min`, `temp_max`: daily temperature variables

## Methods used

- Data wrangling and feature engineering in R
- Exploratory data analysis with `ggplot2`
- Linear regression modelling
- Polynomial temperature effects
- Leave-one-year-out cross-validation
- RMSE, MAE, Dawid-Sebastiani score and interval score
- Residual diagnostics and practical transport-planning interpretation

## Model sequence

- **M0:** baseline model using temperature, weekend indicator and numeric month
- **M1:** adds time trend, month factors and day-of-week factors
- **M2:** adds nonlinear/quadratic temperature effect
- **M3:** adds daily temperature range as an extension

## Selected findings

- Cycling demand shows strong seasonality and day-of-week variation.
- A quadratic temperature effect improves model flexibility and captures diminishing marginal temperature effects.
- M2 and M3 perform similarly in cross-validation; M2 is selected for interpretability and slightly better point forecast metrics.
- Forecast accuracy varies by month: July is predicted best, while April is predicted worst in the final model.
- The fitted linear trend is negative over 2020-2025, but this should be interpreted cautiously because the period includes COVID-era disruption.

## How to run

Open this folder in RStudio, then run:

```r
source("code.R")
```

or knit:

```r
rmarkdown::render("Report_project02.Rmd")
```

Required R packages:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "lubridate", "knitr", "kableExtra", "patchwork", "broom"))
```

## Notes

This is a portfolio adaptation of an academic group project. Student IDs and teammate-identifying details have been removed from the public-facing files. The analysis and report should not be reused as coursework by others.
