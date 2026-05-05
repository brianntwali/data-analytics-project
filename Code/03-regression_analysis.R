library(tidyverse)
library(modelsummary)
library(gt)

# ---------------------------------------------------------------------
# LOAD DATA
cleaned_cases <- read_csv("Cleaned Data/cleaned_cases.csv")

# ---------------------------------------------------------------------
# DESCRIPTIVE REGRESSION ANALYSIS

# Create a binary outcome variables of interest  
  # Whether defendant was assigned a supervision level above the minimum supervision level
  # Whether defendant was assigned a supervision level above/below their recommended level
# And create a month-year FE
cleaned_cases <- cleaned_cases |> 
  mutate(
    ln_bond_amount = log1p(bond_amount),
    assigned_above_level1 = if_else(
      release_decision %in% c("Pretrial Monitoring Level 2, Active", 
                              "Pretrial Monitoring Level 3, Active"), 1, 0),
    decision_month = floor_date(as.Date(decision_date), "month"),
    month_year = factor(format(decision_month, "%Y-%m"))
  )


# OLS NAIVE REGRESSION


# Effect of released w/ conditions vs w/ no conditions on Failure to appear 
fta1 <- feols(
    fta ~ assigned_above_level1 + ln_bond_amount + recommended_release_decision +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket |
    # Court + month_year FEs
    court_name + month_year,
  data = cleaned_cases
)

etable(fta1)

# Effect of harsher than recommended supervision level on Failure to appear 
fta2 <- feols(
  fta ~ assigned_above_recommended + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket |
    # Court + month_year FEs
    court_name + month_year,
  data = cleaned_cases
)

etable(fta2)

# Effect of released w/ conditions vs w/ no conditions on new arrests  
new_arrest1 <- feols(
  new_arrest ~ assigned_above_level1 + ln_bond_amount + recommended_release_decision +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket |
    # Court + month_year FEs
    court_name + month_year,
  data = cleaned_cases
)

etable(new_arrest1)

# Effect of harsher than recommended supervision level on new arrests  
new_arrest2 <- feols(
  new_arrest ~ assigned_above_recommended + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket |
    # Court + month_year FEs
    court_name + month_year,
  data = cleaned_cases
)

etable(new_arrest2)


# TESTING RELEVANCE OF JUDGE FE INSTRUMENT

# Dataset with all case-defendants with judge_name 
cases_nomissing_judge <- cleaned_cases |> filter(!is.na(judge_name))

# WITHOUT JUDGE FE
# Test how much recommended supervision and case specifics predict supervision assignment 

# Assigned above level 1 for supervision
lm_nf_level <- lm(
  assigned_above_level1 ~ recommended_release_decision + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket +
    # Court + month_year FEs
    court_name + month_year,
  data = cases_nomissing_judge
)

# Assigned above recommended supevision level
lm_nf_above <- lm(
  assigned_above_recommended ~ recommended_release_decision + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket +
    # Court + month_year FEs
    court_name + month_year,
  data = cases_nomissing_judge
)

# WITH JUDGE FE
# Test whether judges explain some of the variation 
  # in assignment to above minimum supervision levels and above/below recommended supervision
# If judge FEs are significant, may indicate that judge IV is relevant instrument

lm_fe_level <- lm(
  assigned_above_level1 ~ recommended_release_decision + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket +
    court_name + month_year + judge_name,
  data = cases_nomissing_judge
)

lm_fe_above <- lm(
  assigned_above_recommended ~ recommended_release_decision + ln_bond_amount +
    sexual_offense + domestic_violence_offense + violent_offense + weapons_related_offense +
    most_serious_offense_level + age + offense_bucket +
    # Court + month_year FEs
    court_name + month_year + judge_name,
  data = cases_nomissing_judge
)


# Joint significance tests for judge FE on being assigned above level 1 
anova(lm_nf_level, lm_fe_level) |>
  as.data.frame() |>
  gt() |>
  tab_header(title = "F-test: Joint Significance of Judge FEs",
             subtitle = "Outcome: Assigned Above Level 1") |>
  fmt_number(decimals = 5) |>
  gtsave("Output/Visualizations/anova_level.png")


# Joint significance tests for judge FE on being assigned above recommended level
anova(lm_nf_above, lm_fe_above) |>
  as.data.frame() |>
  gt() |>
  tab_header(title = "F-test: Joint Significance of Judge FEs",
             subtitle = "Outcome: Assigned Above Recommended") |>
  fmt_number(decimals = 5) |>
  gtsave("Output/Visualizations/anova_above.png")



fta_models <- list(
  "FTA: Above Level 1" = fta1,
  "FTA: Above Recommended" = fta2
)

arrest_models <- list(
  "New Arrest: Above Level 1" = new_arrest1,
  "New Arrest: Above Recommended" = new_arrest2
)

ftas <- modelsummary(
  fta_models,
  output = "gt",
  statistic = "({std.error})",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "R2|AIC|BIC|RMSE"
)


gtsave(ftas, "Output/Visualizations/fta_regressions.png")

new_arrests <- modelsummary(
  arrest_models,
  output = "gt",
  statistic = "({std.error})",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "R2|AIC|BIC|RMSE"
)


gtsave(new_arrests, "Output/Visualizations/new_arrests_regressions.png")


