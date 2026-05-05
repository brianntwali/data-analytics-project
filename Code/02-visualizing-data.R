library(tidyverse)
library(ggplot2)
library(summarytools)

# ---------------------------------------------------------------------
# LOAD DATA

cleaned_cases <- read_csv("Cleaned Data/cleaned_cases.csv")


# ---------------------------------------------------------------------
# PRELIMINARY VARIABLE VISUALIZATION
cleaned_cases

# =============================================
# Outcome: Failure to Appear (fta)

cleaned_cases |>
  count(fta, sort = TRUE)

# fta by assigned supervision decision
fta_by_release <- cleaned_cases |>
  group_by(release_decision) |>
  summarize(fta_rate = mean(fta, na.rm = TRUE)) |>
  arrange(fta_rate)

ggplot(fta_by_release, aes(x = release_decision, y = fta_rate)) +
  geom_col() +
  coord_flip() 

# New arrest by crime type 
fta_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(fta_rate = mean(fta, na.rm = TRUE)) |>
  arrange(fta_rate)

ggplot(fta_by_crime, aes(x = offense_bucket, y = fta_rate)) +
  geom_col() +
  coord_flip() 


# =============================================
# Outcome: Sentenced

cleaned_cases |>
  count(sentenced, sort = TRUE)

# Sentenced by assigned supervision decision

sentenced_by_release <- cleaned_cases |>
  group_by(release_decision) |>
  summarize(sentence_rate = mean(sentenced, na.rm = TRUE)) |>
  arrange(sentence_rate)

ggplot(sentenced_by_release, aes(x = release_decision, y = sentence_rate)) +
  geom_col() +
  coord_flip() 

# Sentenced by crime type 

sentenced_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(sentence_rate = mean(sentenced, na.rm = TRUE)) |>
  arrange(sentence_rate)

ggplot(sentenced_by_crime, aes(x = offense_bucket, y = sentence_rate)) +
  geom_col() +
  coord_flip() 


# =============================================
# Outcome: New arrest
cleaned_cases |>
  count(new_arrest, sort = TRUE)


# New arrest by assigned supervision decision
arrest_by_release <- cleaned_cases |>
  group_by(release_decision) |>
  summarize(new_arrest_rate = mean(new_arrest, na.rm = TRUE)) |>
  arrange(new_arrest_rate)

ggplot(arrest_by_release, aes(x = release_decision, y = new_arrest_rate)) +
  geom_col() +
  coord_flip() 

# New arrest by crime type 
arrest_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(new_arrest_rate = mean(new_arrest, na.rm = TRUE)) |>
  arrange(new_arrest_rate)

ggplot(arrest_by_crime, aes(x = offense_bucket, y = new_arrest_rate)) +
  geom_col() +
  coord_flip() 


# =============================================
# Bond 

summarytools::descr(cleaned_cases$bond_amount)

# Use a log1p tranformation to handle skewness while not dropping zeros
cleaned_cases <- cleaned_cases |>
  mutate(
    ln_bond_amount = log1p(bond_amount)
  ) |>
  relocate(
    ln_bond_amount, .after = bond_amount
  )

# Bond amount across assigned supervision decision
bond_by_release <- cleaned_cases |>
  group_by(release_decision) |>
  summarize(avg_bond = mean(bond_amount, na.rm = TRUE)) |>
  arrange(avg_bond)

ggplot(bond_by_release, aes(x = release_decision, y = avg_bond)) +
  geom_col() +
  coord_flip() 

ln_bond_by_release <- cleaned_cases |>
  group_by(release_decision) |>
  summarize(avg_ln_bond = mean(ln_bond_amount, na.rm = TRUE)) |>
  arrange(avg_ln_bond)

ggplot(ln_bond_by_release, aes(x = release_decision, y = avg_ln_bond)) +
  geom_col() +
  coord_flip() 

# bond amount across offense_types

bond_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(avg_bond = mean(bond_amount, na.rm = TRUE)) |>
  arrange(avg_bond)

ggplot(bond_by_crime, aes(x = offense_bucket, y = avg_bond)) +
  geom_col() +
  coord_flip() 

ln_bond_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(avg_ln_bond = mean(ln_bond_amount, na.rm = TRUE)) |>
  arrange(avg_ln_bond)

ggplot(ln_bond_by_crime, aes(x = offense_bucket, y = avg_ln_bond)) +
  geom_col() +
  coord_flip() 

# =============================================
# Assigned release 

freq(cleaned_cases$release_decision)

# assigned release by caseload_name (county)

lvl1_by_county <- cleaned_cases |>
  group_by(caseload_name) |>
  summarize(level_1_rate = mean(release_level_1, na.rm = TRUE)) |>
  arrange(level_1_rate)

ggplot(lvl1_by_county, aes(x = caseload_name, y = level_1_rate)) +
  geom_col() +
  coord_flip() 

lvl2_by_county <- cleaned_cases |>
  group_by(caseload_name) |>
  summarize(level_2_rate = mean(release_level_2, na.rm = TRUE)) |>
  arrange(level_2_rate)

ggplot(lvl2_by_county, aes(x = caseload_name, y = level_2_rate)) +
  geom_col() +
  coord_flip() 

lvl3_by_county <- cleaned_cases |>
  group_by(caseload_name) |>
  summarize(level_3_rate = mean(release_level_3, na.rm = TRUE)) |>
  arrange(level_3_rate)

ggplot(lvl3_by_county, aes(x = caseload_name, y = level_3_rate)) +
  geom_col() +
  coord_flip() 

# assigned release by crime type

lvl1_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(level_1_rate = mean(release_level_1, na.rm = TRUE)) |>
  arrange(level_1_rate)

ggplot(lvl1_by_crime, aes(x = offense_bucket, y = level_1_rate)) +
  geom_col() +
  coord_flip() 

lvl2_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(level_2_rate = mean(release_level_2, na.rm = TRUE)) |>
  arrange(level_2_rate)

ggplot(lvl2_by_crime, aes(x = offense_bucket, y = level_2_rate)) +
  geom_col() +
  coord_flip() 

lvl3_by_crime <- cleaned_cases |>
  group_by(offense_bucket) |>
  summarize(level_3_rate = mean(release_level_3, na.rm = TRUE)) |>
  arrange(level_3_rate)

ggplot(lvl3_by_crime, aes(x = offense_bucket, y = level_3_rate)) +
  geom_col() +
  coord_flip() 


# ---------------------------------------------------------------------
# MAIN FIGURES


# ======================================================================
# Judge by caseload figure

# Make one row per defendant-case and assign judge_id
# ----------------------------------------------------------------------
# 1) One row per defendant-case with cleaned judge name and deviation outcome

cases_judge_level <- cleaned_cases |>
  mutate(
    # clean judge names
    judge_name = str_remove(judge_name, regex("^hon\\.?\\s+", ignore_case = TRUE)),
    judge_name = str_remove(judge_name, regex("^honorable\\s+", ignore_case = TRUE)),
    judge_name = str_remove(judge_name, regex("^judge\\s+", ignore_case = TRUE)),
    judge_name = str_remove_all(judge_name, regex("\\bpro[ -]?tem\\b", ignore_case = TRUE)),
    judge_name = str_remove_all(judge_name, regex("^standing master\\s*", ignore_case = TRUE)),
    judge_name = str_squish(judge_name),
    judge_name = na_if(judge_name, "")
  ) |>
  mutate(
    judge_name = case_when(
      judge_name == "Townsend" ~ "Karen Townsend",
      judge_name == "Larson" ~ "John Larson",
      judge_name == "Deschamps" ~ "Robert Deschamps",
      TRUE ~ judge_name
    )
  ) |>
  group_by(defendant_id_number, case_number) |>
  # Some have duplicates across defendant_id_number and case_number (58 duplicates)
  summarise(
    judge_name = first(na.omit(judge_name)),
    caseload_name = first(caseload_name),
    assigned_level_num = first(assigned_level_num),
    recommended_level_num = first(recommended_level_num),
    assigned_above_recommended = max(assigned_above_recommended, na.rm = TRUE),
    assigned_below_recommended = max(assigned_below_recommended, na.rm = TRUE),
    assigned_equals_recommended = max(assigned_equals_recommended, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    !is.na(judge_name),
    !judge_name %in% c('""', '""""', '""To be assigned""', '"To be assigned"', '"To be  assigned"', "To be assigned", "To be  assigned"),
  ) |>
  mutate(
    judge_id = dense_rank(judge_name)
  )

# Crosswalk
judge_key <- cases_judge_level |>
  distinct(judge_id, judge_name) |>
  arrange(judge_id)

View(judge_key)


# Make plotting data using assignment indicators

judge_plot_data <- cases_judge_level |>
  mutate(
    outcome = case_when(
      assigned_above_recommended == 1 ~ "Assigned Above Recommended",
      assigned_below_recommended == 1 ~ "Assigned Below Recommended",
      assigned_equals_recommended == 1 ~ "Assigned = Recommended"
    )
  ) |>
  filter(!is.na(outcome)) |>
  count(caseload_name, judge_id, outcome, name = "n")

# Order judges within caseload by total cases (so plots are stable)
judge_order <- cases_judge_level |>
  group_by(caseload_name, judge_id) |>
  summarise(total_cases = n(), .groups = "drop") |>
  arrange(caseload_name, total_cases) |>
  pull(judge_id) |>
  unique()

judge_plot_data <- judge_plot_data |>
  mutate(judge_id = factor(judge_id, levels = judge_order))

# Keep judge-caseload cells with more than 30 defendant-case observations
judge_plot_data_30 <- judge_plot_data |>
  group_by(caseload_name, judge_id) |>
  mutate(total_cases = sum(n)) |>
  ungroup() |>
  filter(total_cases > 30) |> 
  mutate(
    outcome = factor(
      outcome,
      # Make sure graphical ordering is logical 
      levels = c(
        "Assigned Below Recommended",
        "Assigned = Recommended",
        "Assigned Above Recommended"
      )
    )
  )


deviation_by_judge_caseload <- ggplot(
  judge_plot_data_30,
  aes(x = judge_id, y = n, fill = outcome)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ caseload_name, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Assigned Below Recommended" = "#4575B4",  
      "Assigned = Recommended"     = "#D9A92A",  
      "Assigned Above Recommended" = "#D73027"   
    )
  ) +
  labs(
    x = "Judge ID",
    y = "Number of defendant-case observations",
    fill = NULL,
    title = "Assigned vs Recommended Supervision Levels by Judge and Caseload"
  ) +
  theme_minimal()

print(deviation_by_judge_caseload)

# ======================================================================
# Outcome distribution

outcome_cases <- cleaned_cases |>
  mutate(
    released_level = case_when(
      release_no_monitoring == 1 ~ "No Monitoring",
      release_level_1 == 1 ~ "Level 1",
      release_level_2 == 1 ~ "Level 2",
      release_level_3 == 1 ~ "Level 3",
      TRUE ~ NA_character_
    ),
    detained_level = case_when(
      detained_no_monitoring == 1 ~ "No Monitoring",
      detained_level_1 == 1 ~ "Level 1",
      detained_level_2 == 1 ~ "Level 2",
      detained_level_3 == 1 ~ "Level 3",
      TRUE ~ NA_character_
    )
  )

# Build one clean summary for Released

released_summary <- outcome_cases |>
  filter(!is.na(released_level)) |>
  group_by(level = released_level) |>
  summarise(
    fta_n = sum(fta == 1, na.rm = TRUE),
    fta_denom = sum(!is.na(fta)),
    fta_rate = mean(fta, na.rm = TRUE),
    
    new_arrest_n = sum(new_arrest == 1, na.rm = TRUE),
    new_arrest_denom = sum(!is.na(new_arrest)),
    new_arrest_rate = mean(new_arrest, na.rm = TRUE),
    
    sentenced_n = sum(sentenced == 1, na.rm = TRUE),
    sentenced_denom = sum(!is.na(sentenced)),
    sentenced_rate = mean(sentenced, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group_type = "Released")

# Build one clean summary for Detained

detained_summary <- outcome_cases |>
  filter(!is.na(detained_level)) |>
  group_by(level = detained_level) |>
  summarise(
    fta_n = sum(fta == 1, na.rm = TRUE),
    fta_denom = sum(!is.na(fta)),
    fta_rate = mean(fta, na.rm = TRUE),
    
    new_arrest_n = sum(new_arrest == 1, na.rm = TRUE),
    new_arrest_denom = sum(!is.na(new_arrest)),
    new_arrest_rate = mean(new_arrest, na.rm = TRUE),
    
    sentenced_n = sum(sentenced == 1, na.rm = TRUE),
    sentenced_denom = sum(!is.na(sentenced)),
    sentenced_rate = mean(sentenced, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group_type = "Detained")

# Combine 

level_outcome_data <- bind_rows(released_summary, detained_summary) |>
  mutate(
    level = factor(level, levels = c("No Monitoring", "Level 1", "Level 2", "Level 3")),
    group_type = factor(group_type, levels = c("Released", "Detained"))
  ) |>
  pivot_longer(
    cols = -c(level, group_type),
    names_to = c("outcome", ".value"),
    names_pattern = "(fta|new_arrest|sentenced)_(n|denom|rate)"
  ) |>
  mutate(
    outcome = recode(
      outcome,
      fta = "FTA",
      new_arrest = "New Arrest",
      sentenced = "Sentenced"
    ),
    outcome = factor(outcome, levels = c("FTA", "New Arrest", "Sentenced"))
  )


# Graph displaying rates
level_outcome_rate_graph <- ggplot(
  level_outcome_data,
  aes(x = outcome, y = rate, fill = level)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~ group_type) +
  scale_fill_manual(
    values = c(
      "No Monitoring" = "#4E79A7",
      "Level 1" = "#59A14F",
      "Level 2" = "#F28E2B",
      "Level 3" = "#E15759"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Outcome Rate",
    fill = "Level",
    title = "FTA, New Arrest, & Sentenced Rates (Released vs Detained Levels)"
  ) +
  theme_minimal()

print(level_outcome_rate_graph)


# Graph displaying counts

level_outcome_count_graph <- ggplot(
  level_outcome_data,
  aes(x = outcome, y = n, fill = level)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~ group_type, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "No Monitoring" = "#4E79A7",
      "Level 1" = "#59A14F",
      "Level 2" = "#F28E2B",
      "Level 3" = "#E15759"
    )
  ) +
  labs(
    x = NULL,
    y = "Count of Positive Outcomes",
    fill = "Level",
    title = "Counts of FTA, New Arrest, & Sentenced Outcomes (Released vs Detained Levels)"
  ) +
  theme_minimal()

print(level_outcome_count_graph)



# Save plots

ggsave("Output/release_deviation_by_judge_caseload.pdf", plot = deviation_by_judge_caseload, width = 12, height = 9)
ggsave("Output/level_outcome_rate_graph.pdf", plot = level_outcome_rate_graph, width = 10, height = 6)
ggsave("Output/level_outcome_count_graph.pdf", plot = level_outcome_count_graph, width = 10, height = 6)

ggsave("Output/release_deviation_by_judge_caseload.png", plot = deviation_by_judge_caseload, width = 12, height = 9)
ggsave("Output/level_outcome_rate_graph.png", plot = level_outcome_rate_graph, width = 10, height = 6)
ggsave("Output/level_outcome_count_graph.png", plot = level_outcome_count_graph, width = 10, height = 6)
