library(readr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(data.table)


cleaned_cases <- fread("Cleaned Data/cleaned_cases.csv")


# ----------------------------------------------------
# Judge by caseload figure

# Make one row per defendant-case and assign judge_id
cases_judge_level <- cleaned_cases |>
  mutate(
    # clean judge names
    judge_name = str_remove(judge_name, regex("^hon\\.?\\s+", ignore_case = TRUE)),
    judge_name = str_remove(judge_name, regex("^honorable\\s+", ignore_case = TRUE)),
    judge_name = str_remove(judge_name, regex("^judge\\s+", ignore_case = TRUE)),
    judge_name = str_remove_all(judge_name, regex("\\bpro[ -]?tem\\b", ignore_case = TRUE)),
    judge_name = str_remove_all(judge_name, regex("^standing master\\s*", ignore_case = TRUE)),
    judge_name = str_squish(judge_name),
    judge_name = na_if(judge_name, ""),
    
    # Link judge names
    judge_name = case_when(
      judge_name == "Townsend" ~ "Karen Townsend",
      judge_name == "Larson" ~ "John Larson",
      judge_name == "Deschamps" ~ "Robert Deschamps",
      TRUE ~ judge_name
    )
  ) |>
  filter(judge_name != '""') |>
  group_by(defendant_id_number, case_number) |>
  summarise(
    judge_name = first(na.omit(judge_name)),
    caseload_name = first(na.omit(caseload_name)),
    detained = max(detained, na.rm = TRUE),
    release_no_monitoring = max(release_no_monitoring, na.rm = TRUE),
    release_level_1 = max(release_level_1, na.rm = TRUE),
    release_level_2 = max(release_level_2, na.rm = TRUE),
    release_level_3 = max(release_level_3, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    detained = ifelse(is.infinite(detained), NA, detained),
    release_no_monitoring = ifelse(is.infinite(release_no_monitoring), NA, release_no_monitoring),
    release_level_1 = ifelse(is.infinite(release_level_1), NA, release_level_1),
    release_level_2 = ifelse(is.infinite(release_level_2), NA, release_level_2),
    release_level_3 = ifelse(is.infinite(release_level_3), NA, release_level_3)
  ) |>
  filter(
    !is.na(judge_name),
    !judge_name %in% c('""', '""""', '""To be assigned""',  '"To be assigned"', '"To be  assigned"', "To be assigned", "To be  assigned")
  ) |>
  arrange(judge_name) |>
  mutate(
    judge_id = dense_rank(judge_name)
  )

# Crosswalk
judge_key <- cases_judge_level |>
  distinct(judge_id, judge_name) |>
  arrange(judge_id)

View(judge_key)


# Make plotting data using treatment indicators

judge_plot_data <- cases_judge_level |>
  mutate(
    outcome = case_when(
      detained == 1 ~ "Detention",
      detained == 0 ~ "Release",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(outcome), !is.na(caseload_name)) |>
  count(caseload_name, judge_id, outcome, name = "n")

# Order judges by total number of defendant-case observations
judge_order <- cases_judge_level |>
  mutate(
    any_outcome = case_when(
      detained %in% c(0, 1) ~ 1,
      TRUE ~ 0
    )
  ) |>
  filter(!is.na(caseload_name)) |>
  group_by(caseload_name, judge_id) |>
  summarise(total_cases = sum(any_outcome, na.rm = TRUE), .groups = "drop") |>
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
  filter(total_cases > 30)

# Plot
release_detention_by_judge_caseload <- ggplot(judge_plot_data_30, aes(x = judge_id, y = n, fill = outcome)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ caseload_name, scales = "free_y") +
  labs(
    x = "Judge ID",
    y = "Number of defendant-case observations",
    fill = NULL,
    title = "Release and Detention Outcomes by Judge and Caseload"
  ) +
  theme_minimal()

print(release_detention_by_judge_caseload)



# ----------------------------------------------------
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

# Combine and reshape once

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


# Rate graph
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
    title = "FTA, New Arrest, and Sentenced Rates Across Released and Detained Levels"
  ) +
  theme_minimal()

print(level_outcome_rate_graph)


# Count graph

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
    title = "Counts of FTA, New Arrest, and Sentenced Outcomes Across Released and Detained Levels"
  ) +
  theme_minimal()

print(level_outcome_count_graph)



# Save plots

ggsave("Output/release_detention_by_judge_caseload.pdf", plot = release_detention_by_judge_caseload, width = 12, height = 9)
ggsave("Output/level_outcome_rate_graph.pdf", plot = level_outcome_rate_graph, width = 10, height = 6)
ggsave("Output/level_outcome_count_graph.pdf", plot = level_outcome_count_graph, width = 10, height = 6)
