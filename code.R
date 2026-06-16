
# ============================================================
# Project 2: Modelling daily cycling demand in Edinburgh
# Authors: Yixin Sun and project teammates (student IDs removed for privacy)
# ============================================================

# 0. Packages
library(ggplot2); library(dplyr); library(tidyr)
library(lubridate); library(knitr); library(kableExtra)
library(patchwork); library(broom)

# 1. Load data
load('cycle_daily_df.Rdata')

# 2. Data Wrangling 
cycle_daily_df <- cycle_daily_df %>%
  mutate(
    # Task 1: month as ordered factor for plots/inference
    month_f = factor(
      month,
      levels = 1:12,
      labels = month.abb,
      ordered = TRUE
    ),
    # Task 2: dow with explicit levels
    dow_plot = factor(
      dow,
      levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
      ordered = TRUE
    ),
    dow_model = factor(
      dow,
      levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
      ordered = FALSE
    ),
    # Task 3: trend as integer days since 2020-01-01
    trend = as.integer(date - as.Date("2020-01-01")),
    # other directions for m3
    log_count = log(count + 1),
    is_covid = ifelse(year %in% c(2020, 2021), 1, 0),
    is_summer = ifelse(month %in% c(6, 7, 8), 1, 0),
    #final m3 selection
    temp_range = temp_max - temp_min
  )

# Summary statistics table for key continuous variables
eda_summary <- tibble(
  Statistic = c("N", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
  count = c(
    sum(!is.na(cycle_daily_df$count)),
    mean(cycle_daily_df$count, na.rm = TRUE),
    sd(cycle_daily_df$count, na.rm = TRUE),
    min(cycle_daily_df$count, na.rm = TRUE),
    quantile(cycle_daily_df$count, 0.25, na.rm = TRUE),
    median(cycle_daily_df$count, na.rm = TRUE),
    quantile(cycle_daily_df$count, 0.75, na.rm = TRUE),
    max(cycle_daily_df$count, na.rm = TRUE)
  ),
  temp_mean = c(
    sum(!is.na(cycle_daily_df$temp_mean)),
    mean(cycle_daily_df$temp_mean, na.rm = TRUE),
    sd(cycle_daily_df$temp_mean, na.rm = TRUE),
    min(cycle_daily_df$temp_mean, na.rm = TRUE),
    quantile(cycle_daily_df$temp_mean, 0.25, na.rm = TRUE),
    median(cycle_daily_df$temp_mean, na.rm = TRUE),
    quantile(cycle_daily_df$temp_mean, 0.75, na.rm = TRUE),
    max(cycle_daily_df$temp_mean, na.rm = TRUE)
  ),
  temp_min = c(
    sum(!is.na(cycle_daily_df$temp_min)),
    mean(cycle_daily_df$temp_min, na.rm = TRUE),
    sd(cycle_daily_df$temp_min, na.rm = TRUE),
    min(cycle_daily_df$temp_min, na.rm = TRUE),
    quantile(cycle_daily_df$temp_min, 0.25, na.rm = TRUE),
    median(cycle_daily_df$temp_min, na.rm = TRUE),
    quantile(cycle_daily_df$temp_min, 0.75, na.rm = TRUE),
    max(cycle_daily_df$temp_min, na.rm = TRUE)
  ),
  temp_max = c(
    sum(!is.na(cycle_daily_df$temp_max)),
    mean(cycle_daily_df$temp_max, na.rm = TRUE),
    sd(cycle_daily_df$temp_max, na.rm = TRUE),
    min(cycle_daily_df$temp_max, na.rm = TRUE),
    quantile(cycle_daily_df$temp_max, 0.25, na.rm = TRUE),
    median(cycle_daily_df$temp_max, na.rm = TRUE),
    quantile(cycle_daily_df$temp_max, 0.75, na.rm = TRUE),
    max(cycle_daily_df$temp_max, na.rm = TRUE)
  )
)

# 2. Exploratory Data Analysis (EDA)
#summary statistics
eda_summary %>%
  kable(
    digits = 2,
    caption = "Summary statistics for daily cyclist count and temperature variables."
  ) %>%
  kable_styling(full_width = FALSE)
eda_summary

#time series plot
p_ts <- ggplot(cycle_daily_df, aes(x = date, y = count)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(
    title = "Figure 1: Daily cyclist counts in Edinburgh, 2020--2025",
    x = "Date",
    y = "Daily cyclist count",
    caption = "Daily cyclist counts over time with a smooth trend highlighting long term changes."
  )
p_ts

#boxplot by month
p_box_month <- ggplot(cycle_daily_df, aes(x = month_f, y = count)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Figure 2: Distribution of daily cyclist counts by month",
    x = "Month",
    y = "Daily cyclist count",
    caption = "Boxplots of daily cyclist counts by month, showing seasonal differences in central tendency and variability."
  )
p_box_month

#boxplot by day of week
p_box_dow <- ggplot(cycle_daily_df, aes(x = dow_plot, y = count)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Figure 3: Distribution of daily cyclist counts by day of week",
    x = "Day of week",
    y = "Daily cyclist count",
    caption = "Boxplots of daily cyclist counts by day of week, showing systematic within-week variation."
  )
p_box_dow

#scatter plot of count vs temp_mean
p_scatter_temp <- ggplot(cycle_daily_df, aes(x = temp_mean, y = count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(
    title = "Figure 4: Daily cyclist count against mean temperature",
    x = "Daily mean temperature (°C)",
    y = "Daily cyclist count",
    caption = "Scatter plot of cyclist count against mean temperature, to assess whether the temperature effect appears nonlinear."
  )
p_scatter_temp

#average daily cyclist count for each month by year.
monthly_mean_df <- cycle_daily_df %>%
  group_by(year, month_f) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    .groups = "drop"
  )

p_monthly_mean_year <- ggplot(monthly_mean_df, aes(x = month_f, y = mean_count, group = factor(year), colour = factor(year))) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(
    title = "Figure 5: Average daily cyclist count by month and year",
    x = "Month",
    y = "Average daily cyclist count",
    colour = "Year",
    caption = "Monthly mean cyclist counts by year, showing seasonality and possible COVID period disruption."
  )
p_monthly_mean_year

# 3. Model Fitting
#chekclist and navigation
# 3.1 Baseline Model (M0)
#   3.1.1 M0 fit
#   3.1.2 M0 coefficient table
#   3.1.3 M0 diagnostic plots
# 3.2 Proposed Models (M1-M3)
#   3.2.1 M1 fit
#   3.2.2 M2 fit
#   3.2.3 M3 fit (final chosen extension: daily temperature rnge)
#   3.2.4 coefficient tables
#   3.2.5 diagnostic plots
#   3.2.6 exploratory M3 variants tested but not selected


# 3.1.1 M0 fit
m0 <- lm(count ~ temp_mean + weekend + month, data = cycle_daily_df)

# 3.2.1 M1 fit
m1 <- lm(count ~ temp_mean + weekend + trend + factor(month) + dow_model,
         data = cycle_daily_df)

# 3.2.2 M2 fit
m2 <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend +
           factor(month) + dow_model,
         data = cycle_daily_df)

# 3.2.3 M3 fit (final chosen extension: daily temperature rnge)
m3 <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend +
           factor(month) + dow_model + temp_range,
         data = cycle_daily_df)

# 3.2.6 exploratory M3 variants (tested but not selected) log model
m3_log <- lm(log_count ~ temp_mean + I(temp_mean^2) + weekend + trend + 
               factor(month) + dow_model,
             data = cycle_daily_df)

# 3.2.6 exploratory M3 variants (tested but not selected)COVID model
m3_covid <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend +
                 factor(month) + dow_model + is_covid,
               data = cycle_daily_df)

# 3.2.6 exploratory M3 variants (tested but not selected)temperature × summer interaction
m3_interaction <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend +
                       factor(month) + dow_model + temp_mean:is_summer,
                     data = cycle_daily_df)

# 3.2.4 coefficient tables
clean_terms <- function(x) {
  x %>%
    mutate(term = ifelse(term == "(Intercept)", "Intercept", term))
}

m0_coef <- tidy(m0, conf.int = TRUE) %>% clean_terms()
m1_coef <- tidy(m1, conf.int = TRUE) %>% clean_terms()
m2_coef <- tidy(m2, conf.int = TRUE) %>% clean_terms()
m3_coef <- tidy(m3, conf.int = TRUE) %>% clean_terms()

m0_coef %>%
  kable(digits = 3, caption = "Coefficient table for M0.") %>%
  kable_styling(full_width = FALSE)

m1_coef %>%
  kable(digits = 3, caption = "Coefficient table for M1.") %>%
  kable_styling(full_width = FALSE)

m2_coef %>%
  kable(digits = 3, caption = "Coefficient table for M2.") %>%
  kable_styling(full_width = FALSE)

m3_coef %>%
  kable(digits = 3, caption = "Coefficient table for M3.") %>%
  kable_styling(full_width = FALSE)

# 3.2.5 diagnostic plots
plot_model_diagnostics <- function(model, model_name, data) {
  aug <- augment(model, data = data)
  
  caption_text <- switch(
    model_name,
    "M0" = "Figure 6: Diagnostic plots for M0. ",
    "M1" = "Figure 7: Diagnostic plots for M1. ",
    "M2" = "Figure 8: Diagnostic plots for M2. ",
    "M3" = "Figure 9: Diagnostic plots for M3. "
  )
  
  p1 <- ggplot(aug, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(se = FALSE) +
    theme_bw() +
    labs(
      title = paste(model_name, ": Residuals vs fitted"),
      x = "Fitted values",
      y = "Residuals"
    )
  
  p2 <- ggplot(aug, aes(sample = .std.resid)) +
    stat_qq(alpha = 0.5) +
    stat_qq_line() +
    theme_bw() +
    labs(
      title = paste(model_name, ": Normal Q-Q plot"),
      x = "Theoretical quantiles",
      y = "Standardised residuals"
    )
  
  p3 <- ggplot(aug, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    theme_bw() +
    labs(
      title = paste(model_name, ": Scale-location"),
      x = "Fitted values",
      y = expression(sqrt("|Standardised residuals|"))
    )
  
  p4 <- ggplot(aug, aes(x = date, y = .resid)) +
    geom_line(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    labs(
      title = paste(model_name, ": Residuals over time"),
      x = "Date",
      y = "Residuals"
    )
  
  ((p1 | p2) / (p3 | p4)) +
    plot_annotation(
      caption = caption_text
    ) &
    theme(
      plot.caption = element_text(hjust = 0.5, size = 10)
    )
}

diag_m0 <- plot_model_diagnostics(m0, "M0", cycle_daily_df)
diag_m1 <- plot_model_diagnostics(m1, "M1", cycle_daily_df)
diag_m2 <- plot_model_diagnostics(m2, "M2", cycle_daily_df)
diag_m3 <- plot_model_diagnostics(m3, "M3", cycle_daily_df)

diag_m0
diag_m1
diag_m2
diag_m3

#model summary tables

glance(m0) %>%
  kable(digits = 3, caption = "Model summary statistics for M0.") %>%
  kable_styling(full_width = FALSE)

glance(m1) %>%
  kable(digits = 3, caption = "Model summary statistics for M1.") %>%
  kable_styling(full_width = FALSE)

glance(m2) %>%
  kable(digits = 3, caption = "Model summary statistics for M2.") %>%
  kable_styling(full_width = FALSE)

glance(m3) %>%
  kable(digits = 3, caption = "Model summary statistics for M3(temperature range extension).") %>%
  kable_styling(full_width = FALSE)

# 4. Cross-Validation Functions (table 1 and 2)
#scoring function
calc_scores <- function(y, mu, sigma, alpha = 0.05) {
  # y     : vector of observed values
  # mu    : vector of predictive means (from predict(fit, newdata=test)$fit)
  # sigma : vector of predictive SDs. Combine residual error and mean uncertainty:
  #         sigma = sqrt(summary(fit)$sigma^2 + predict(fit, newdata=test, se.fit=TRUE)$se.fit^2)
  # Returns a named list with RMSE, MAE, DS, IS
  
  # Implement RMSE, MAE, DS, and IS: 
  rmse <- sqrt(mean((y - mu)^2, na.rm = TRUE))
  mae  <- mean(abs(y - mu), na.rm = TRUE)
  
  ds <- mean(log(sigma^2) + ((y - mu)^2 / sigma^2), na.rm = TRUE)
  
  z <- qnorm(1 - alpha / 2)
  l <- mu - z * sigma
  u <- mu + z * sigma
  
  is <- mean(
    (u - l) +
      (2 / alpha) * pmax(l - y, 0) +
      (2 / alpha) * pmax(y - u, 0),
    na.rm = TRUE
  )
  
  tibble(
    RMSE = rmse,
    MAE  = mae,
    DS   = ds,
    IS   = is
  )
}
calc_scores(
  y = c(10, 12, 15),
  mu = c(11, 11, 14),
  sigma = c(2, 2, 2)
)

# 5. Leave-One-Year-Out CV Loop 
#m0 leave one year out CV 
years <- sort(unique(cycle_daily_df$year))

cv_m0_list <- list()

for (yr in years) {
  train <- cycle_daily_df %>% filter(year != yr)
  test  <- cycle_daily_df %>% filter(year == yr)
  
  fit_m0_cv <- lm(count ~ temp_mean + weekend + month, data = train)
  
  pred <- predict(fit_m0_cv, newdata = test, se.fit = TRUE)
  sigma <- sqrt(summary(fit_m0_cv)$sigma^2 + pred$se.fit^2)
  
  fold_scores <- calc_scores(
    y = test$count,
    mu = pred$fit,
    sigma = sigma
  ) %>%
    mutate(HoldoutYear = yr)
  
  cv_m0_list[[as.character(yr)]] <- fold_scores
}

cv_m0_yearly <- bind_rows(cv_m0_list) %>%
  select(HoldoutYear, RMSE, MAE, DS, IS)

cv_m0_yearly

cv_m0_summary <- cv_m0_yearly %>%
  summarise(
    RMSE = mean(RMSE),
    MAE  = mean(MAE),
    DS   = mean(DS),
    IS   = mean(IS)
  )

cv_m0_summary

#m1 leave one year out CV 
cv_m1_list <- list()

for (yr in years) {
  train <- cycle_daily_df %>% filter(year != yr)
  test  <- cycle_daily_df %>% filter(year == yr)
  
  # CV version uses numeric month instd of factor(month)
  fit_m1_cv <- lm(count ~ temp_mean + weekend + trend + month + dow_model, data = train)
  
  pred <- predict(fit_m1_cv, newdata = test, se.fit = TRUE)
  sigma <- sqrt(summary(fit_m1_cv)$sigma^2 + pred$se.fit^2)
  
  fold_scores <- calc_scores(
    y = test$count,
    mu = pred$fit,
    sigma = sigma
  ) %>%
    mutate(HoldoutYear = yr)
  
  cv_m1_list[[as.character(yr)]] <- fold_scores
}

cv_m1_yearly <- bind_rows(cv_m1_list) %>%
  select(HoldoutYear, RMSE, MAE, DS, IS)

cv_m1_yearly

cv_m1_summary <- cv_m1_yearly %>%
  summarise(
    RMSE = mean(RMSE),
    MAE  = mean(MAE),
    DS   = mean(DS),
    IS   = mean(IS)
  )

cv_m1_summary

#m2 leave one year out CV 
cv_m2_list <- list()

for (yr in years) {
  train <- cycle_daily_df %>% filter(year != yr)
  test  <- cycle_daily_df %>% filter(year == yr)
  
  fit_m2_cv <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend + month + dow_model,
                  data = train)
  
  pred <- predict(fit_m2_cv, newdata = test, se.fit = TRUE)
  sigma <- sqrt(summary(fit_m2_cv)$sigma^2 + pred$se.fit^2)
  
  fold_scores <- calc_scores(
    y = test$count,
    mu = pred$fit,
    sigma = sigma
  ) %>%
    mutate(HoldoutYear = yr)
  
  cv_m2_list[[as.character(yr)]] <- fold_scores
}

cv_m2_yearly <- bind_rows(cv_m2_list) %>%
  select(HoldoutYear, RMSE, MAE, DS, IS)

cv_m2_yearly

cv_m2_summary <- cv_m2_yearly %>%
  summarise(
    RMSE = mean(RMSE),
    MAE  = mean(MAE),
    DS   = mean(DS),
    IS   = mean(IS)
  )

cv_m2_summary

#m3 leave one year out CV 
cv_m3_list <- list()

for (yr in years) {
  train <- cycle_daily_df %>% filter(year != yr)
  test  <- cycle_daily_df %>% filter(year == yr)
  
  fit <- lm(count ~ temp_mean + I(temp_mean^2) + weekend + trend + month + dow_model + temp_range,
            data = train)
  
  pred <- predict(fit, newdata = test, se.fit = TRUE)
  sigma <- sqrt(summary(fit)$sigma^2 + pred$se.fit^2)
  
  cv_m3_list[[as.character(yr)]] <- calc_scores(
    y = test$count,
    mu = pred$fit,
    sigma = sigma
  ) %>%
    mutate(HoldoutYear = yr)
}

cv_m3_yearly <- bind_rows(cv_m3_list)

cv_m3_summary <- cv_m3_yearly %>%
  summarise(
    RMSE = mean(RMSE),
    MAE  = mean(MAE),
    DS   = mean(DS),
    IS   = mean(IS)
  )

cv_m3_yearly
cv_m3_summary

# table 1 cv scores for m0 to m3
table1_cv <- bind_rows(
  cv_m0_summary %>% mutate(Model = "M0"),
  cv_m1_summary %>% mutate(Model = "M1"),
  cv_m2_summary %>% mutate(Model = "M2"),
  cv_m3_summary %>% mutate(Model = "M3"),
) %>%
  select(Model, RMSE, MAE, DS, IS)

table1_cv

table1_cv %>%
  kable(
    digits = c(0, 1, 1, 2, 1),
    caption = "Leave-one-year-out CV scores for M0-M3. Lower is better for all scores."
  ) %>%
  kable_styling(full_width = FALSE)

# 6. CV by Month 
months <- 1:12
cv_month_list <- list()

for (m in months) {
  train <- cycle_daily_df %>% filter(month != m)
  test  <- cycle_daily_df %>% filter(month == m)
  
  fit_final_cv <- lm(
    count ~ temp_mean + I(temp_mean^2) + weekend + trend + month + dow_model,
    data = train
  )
  
  pred <- predict(fit_final_cv, newdata = test, se.fit = TRUE)
  sigma <- sqrt(summary(fit_final_cv)$sigma^2 + pred$se.fit^2)
  
  fold_scores <- calc_scores(
    y = test$count,
    mu = pred$fit,
    sigma = sigma
  ) %>%
    mutate(MonthNum = m)
  
  cv_month_list[[as.character(m)]] <- fold_scores
}

table2_month_cv <- bind_rows(cv_month_list) %>%
  mutate(
    Month = factor(MonthNum, levels = 1:12, labels = month.abb, ordered = TRUE)
  ) %>%
  select(Month, RMSE, DS) %>%
  arrange(Month)

table2_month_cv %>%
  kable(
    digits = c(0, 1, 2),
    caption = "CV RMSE and DS by month for the final model (M2)."
  ) %>%
  kable_styling(full_width = FALSE)

# Q5B practical performance
#5.1 temperature effects and nonlinearity
#fit M2 three times using temp_mean, temp_min, temp_max
temp_specs_m2 <- list(
  temp_mean = count ~ temp_mean + I(temp_mean^2) + weekend + trend + month + dow_model,
  temp_min  = count ~ temp_min  + I(temp_min^2)  + weekend + trend + month + dow_model,
  temp_max  = count ~ temp_max  + I(temp_max^2)  + weekend + trend + month + dow_model
)

temp_cv_results <- vector("list", length(temp_specs_m2))
names(temp_cv_results) <- names(temp_specs_m2)

for (temp_name in names(temp_specs_m2)) {
  fold_list <- vector("list", length(years))
  names(fold_list) <- as.character(years)
  
  for (yr in years) {
    train <- cycle_daily_df %>% filter(year != yr)
    test  <- cycle_daily_df %>% filter(year == yr)
    
    fit <- lm(temp_specs_m2[[temp_name]], data = train)
    pred <- predict(fit, newdata = test, se.fit = TRUE)
    sigma <- sqrt(summary(fit)$sigma^2 + pred$se.fit^2)
    
    fold_list[[as.character(yr)]] <- calc_scores(
      y = test$count,
      mu = pred$fit,
      sigma = sigma
    )
  }
  
  temp_cv_results[[temp_name]] <- bind_rows(fold_list) %>%
    summarise(
      RMSE = mean(RMSE),
      DS   = mean(DS),
      IS   = mean(IS)
    ) %>%
    mutate(Temperature = temp_name)
}

table_temp_compare <- bind_rows(temp_cv_results) %>%
  select(Temperature, RMSE, DS, IS)

table_temp_compare %>%
  kable(
    digits = c(0, 1, 2, 1),
    caption = "CV comparison of M2 fitted with alternative temperature variables."
  ) %>%
  kable_styling(full_width = FALSE)

table_temp_compare

# base on cv table temp_max gives the best predictive performance
m2_tempmax <- lm(
  count ~ temp_max + I(temp_max^2) + weekend + trend + factor(month) + dow_model,
  data = cycle_daily_df
)
#marginal effect
b1 <- unname(coef(m2_tempmax)["temp_max"])
b2 <- unname(coef(m2_tempmax)["I(temp_max^2)"])

marginal_5  <- b1 + 2 * b2 * 5
marginal_15 <- b1 + 2 * b2 * 15

table_temp_marginal <- tibble(
  Evaluation_point = c("5\u00B0C", "15\u00B0C"),
  marginal_effect = c(marginal_5, marginal_15)
)

table_temp_marginal %>%
  kable(
    digits = c(0, 1),
    col.names = c("Evaluation point", "Marginal effect (cyclists per 1\u00B0C)"),
    caption = "Estimated marginal temperature effect for the selected M2 variant."
  ) %>%
  kable_styling(full_width = FALSE)
# 5.3 longterm trend and extrapolation
#table 3 trend coefficients
get_trend_row <- function(model, model_name) {
  daily_change <- unname(coef(model)["trend"])
  
  tibble(
    Model = model_name,
    `Daily change (cyclists/day)` = daily_change,
    `Annual change (cyclists/year)` = daily_change * 365
  )
}

table3_trend <- bind_rows(
  get_trend_row(m1, "M1"),
  get_trend_row(m2, "M2"),
  get_trend_row(m3, "M3")
)

table3_trend %>%
  kable(
    digits = c(0, 3, 0),
    caption = "Table 3: Trend coefficients from M1-M3."
  ) %>%
  kable_styling(full_width = FALSE)

#trend significance for final model
trend_sig_m2 <- tidy(m2, conf.int = TRUE) %>%
  filter(term == "trend") %>%
  transmute(
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  )

trend_sig_m2 %>%
  kable(
    digits = 4,
    caption = "Trend inference for the final model."
  ) %>%
  kable_styling(full_width = FALSE)

#annual change as % of predicted count at start of observation period
start_date_data <- cycle_daily_df %>%
  filter(date == as.Date("2020-01-01")) %>%
  slice(1)

baseline_2020 <- tibble(
  date = as.Date("2020-01-01"),
  year = 2020,
  month = start_date_data$month,
  weekend = start_date_data$weekend,
  dow_model = factor(as.character(start_date_data$dow_model),
                     levels = levels(cycle_daily_df$dow_model)),
  trend = 0,
  temp_mean = start_date_data$temp_mean
)

start_pred <- predict(m2, newdata = baseline_2020)

annual_change_m2 <- unname(coef(m2)["trend"]) * 365
annual_pct_m2 <- 100 * annual_change_m2 / start_pred

annual_change_summary <- tibble(
  `Predicted count at 2020-01-01` = start_pred,
  `Annual change (cyclists/year)` = annual_change_m2,
  `Annual percentage change` = annual_pct_m2
)

annual_change_summary %>%
  kable(
    digits = c(1, 0, 2),
    caption = "Annual change implied by the final model, relative to 1 January 2020."
  ) %>%
  kable_styling(full_width = FALSE)

# compare m1 m2 m3 trend estimates
trend_consistency <- bind_rows(
  tidy(m1, conf.int = TRUE) %>% filter(term == "trend") %>% mutate(Model = "M1"),
  tidy(m2, conf.int = TRUE) %>% filter(term == "trend") %>% mutate(Model = "M2"),
  tidy(m3, conf.int = TRUE) %>% filter(term == "trend") %>% mutate(Model = "M3")
) %>%
  select(Model, estimate, std.error, conf.low, conf.high, p.value)

trend_consistency %>%
  kable(
    digits = 4,
    caption = "Comparison of trend estimates across M1, M2, and M3."
  ) %>%
  kable_styling(full_width = FALSE)

#projection to 2035 under fixed summer weekday conditions
projection_df <- tibble(
  date = seq(as.Date("2020-01-01"), as.Date("2035-12-31"), by = "day")
) %>%
  mutate(
    year = year(date),
    month = 7,
    weekend = 0,
    dow_model = factor("Wed", levels = levels(cycle_daily_df$dow_model)),
    trend = as.integer(date - as.Date("2020-01-01")),
    temp_mean = mean(cycle_daily_df$temp_mean[cycle_daily_df$month %in% c(6, 7, 8)], na.rm = TRUE)
  )

proj_pred <- predict(m2, newdata = projection_df, se.fit = TRUE)

projection_df <- projection_df %>%
  mutate(
    fit = proj_pred$fit,
    lwr = fit - 1.96 * sqrt(summary(m2)$sigma^2 + proj_pred$se.fit^2),
    upr = fit + 1.96 * sqrt(summary(m2)$sigma^2 + proj_pred$se.fit^2)
  )

p_projection <- ggplot(projection_df, aes(x = date, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  theme_bw() +
  labs(
    title = "Figure 10: Projected daily cycling demand to 2035",
    x = "Date",
    y = "Predicted daily cyclist count"
  )

p_projection

#thresholds: here we do two thresholds, 15000 as hinted in the instructions gives a rather early threshold date, so we tested with another threshold since the linear trend is declining 
threshold <- 15000

threshold_hits <- projection_df %>%
  filter(fit <= threshold)

threshold_date <- if (nrow(threshold_hits) == 0) {
  NA
} else {
  min(threshold_hits$date)
}

threshold_date

threshold <- 10000

threshold_hits <- projection_df %>%
  filter(fit <= threshold)

threshold_date <- if (nrow(threshold_hits) == 0) {
  NA
} else {
  min(threshold_hits$date)
}

threshold_date
# 5.4 identify poorly fit period
#locate worst month/period from residuals
aug_m2 <- augment(m2, data = cycle_daily_df)

monthly_resid_summary <- aug_m2 %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(
    mean_residual = mean(.resid, na.rm = TRUE),
    rmse = sqrt(mean(.resid^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(rmse))

monthly_resid_summary %>%
  kable(
    digits = c(0, 1, 1),
    caption = "Monthly residual summary for the final model (M2), ordered by RMSE."
  ) %>%
  kable_styling(full_width = FALSE)

# pick the worst fit month
worst_period <- monthly_resid_summary %>% slice(1)
worst_period_label <- worst_period$year_month

worst_start <- as.Date(paste0(worst_period_label, "-01"))
worst_end <- ceiling_date(worst_start, unit = "month") - days(1)

worst_period_stats <- aug_m2 %>%
  filter(date >= worst_start, date <= worst_end) %>%
  summarise(
    mean_residual = mean(.resid, na.rm = TRUE),
    rmse = sqrt(mean(.resid^2, na.rm = TRUE))
  ) %>%
  mutate(
    period = worst_period_label,
    direction = case_when(
      mean_residual > 0 ~ "Under-prediction on average",
      mean_residual < 0 ~ "Over-prediction on average",
      TRUE ~ "No systematic bias on average"
    )
  ) %>%
  select(period, mean_residual, rmse, direction)

worst_period_stats %>%
  kable(
    digits = c(0, 1, 1, 0),
    caption = "Residual summary for the single worst fit period identified from the final model (M2)."
  ) %>%
  kable_styling(full_width = FALSE)

p_resid_time <- ggplot(aug_m2, aes(x = date, y = .resid)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_rect(
    aes(
      xmin = worst_start,
      xmax = worst_end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  theme_bw() +
  labs(
    title = "Figure 11: Residuals over time for the final model (M2)",
    x = "Date",
    y = "Residual",
    caption = paste(
      "Residuals over time for M2,",
      worst_period_label,
      "highlighted as the worst fit month. Positive residuals indicate under prediction."
    )
  )

p_resid_time

# 5.5 Original diagnostic / insight plot
# residuals heatmap to inspect bias
resid_heatmap_df <- aug_m2 %>%
  mutate(
    Year = factor(year(date)),
    Month = factor(month(date), levels = 1:12, labels = month.abb, ordered = TRUE)
  ) %>%
  group_by(Year, Month) %>%
  summarise(
    mean_residual = mean(.resid, na.rm = TRUE),
    .groups = "drop"
  )

p_resid_heatmap <- ggplot(resid_heatmap_df, aes(x = Month, y = Year, fill = mean_residual)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_residual, 0)), size = 3) +
  theme_bw() +
  labs(
    title = "Figure 12: Systematic prediction bias by month and year for the final model (M2)",
    x = "Month",
    y = "Year",
    fill = "Mean residual",
    caption = "Mean residuals by month and year for the final model. Forecast bias is not uniform over time."
  )

p_resid_heatmap