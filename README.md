# Data folder

The raw dataset is intentionally not included in this public portfolio version.

To reproduce the analysis, place the original file here:

```text
data/raw/cycle_daily_df.Rdata
```

The R object should be named `cycle_daily_df` and contain the columns described in `data_dictionary.md`.

## R packages

Install the packages below before running the analysis:

```r
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "lubridate",
  "broom", "patchwork", "readr"
))
```
