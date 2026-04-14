# ============================================
# MERGING PRETRIAL DATA
# ============================================


all_cases <- read_csv("All_Cases.csv") |> 
  clean_names() |> 
  rename(case_number = cause_number) |> 
  relocate(judicial_override, .after = "supervision_level") |> 
  # There are 1824 missing judge name entries, will track down later
  filter(!judge_name %in% c('"To be assigned"', '"To be  assigned"')) |>
  # There are 1,131 NAs judge names
  filter(!is.na(judge_name)) |> 
  filter(case_number != "To be deleted")

# 10% sample
cases_sample <- all_cases |> 
  slice_sample(prop = 0.1)

View(cases_sample)

names(cases_sample)


# I have to make sure my descriptives are robust to excluding bump ups
