library(readr)
library(janitor)
library(tidyverse)
library(data.table)

# ====================================================
# EXPLORING PRETRIAL DATA
# ====================================================

all_cases <- read_csv("Data/All_Cases.csv") |> 
  # Make all variable names to snake_case
  clean_names() |> 
  relocate(judicial_override, .after = "supervision_level") |> 
  # Rename incorrectly named variable
  rename(case_number = cause_number) |> 
  # Drop alternative defendant id
  select(-defendant_id) 


skimr::skim(all_cases)


clean_cases <- all_cases |> 
  # Drop all-NA or nearly all-NA columns 
  select(
    -recommended_bond_amount,
    - surety_contact_info,
    -qualifiers,
    -surety_company,
    -bondsman
  ) |> 
  # Treat id variables as characters
  mutate(
    pretrial_case_id = as.character(pretrial_case_id),
    defendant_id_number = as.character(defendant_id_number)
  ) |> 
  # Drop counties failed to correctly implement program per pretrial program manager
  filter(
    !caseload_name %in% c("Central Office", "Ravalli", "Broadwater County")
  ) |> 
  # Drop cases that never used the PSA and could be wonky (per case manager)
  filter(!str_detect(case_number, "PC") & 
           !recommended_release_decision == "None") |> 
  # Drop cases marked to be deleted while keeping ones with NA arrest_number
  filter(is.na(arrest_number) | !str_detect(arrest_number, "DELETE")) |> 
  # Drop cases with "bad" case numbers
  filter(
    !is.na(case_number),
    !case_number %in% c("ERROR", "Error01", "To be deleted", "To Be Deleted")
  ) |> 
  # Drop Municipal court cases as they don't use PSAs
  filter(!str_detect(court_name, "Municipal Court")) |> 
  # Drop cases that were dismissed due to death of defendants
  filter(!str_detect(disposition_reasons, "Death"))

View(clean_cases)



skimr::skim(clean_cases)


# Investigate negative days_on_supervision and days_open

View(clean_cases |> 
       filter(days_on_supervision <= 0))

# Correcting three of the four observations returned:
clean_cases <- clean_cases |>
  mutate(
    supervision_start_date = case_when(
      defendant_id_number == "221409" & days_on_supervision < 0 ~ 
        as.POSIXct("2023-08-08 06:00:00"),
      defendant_id_number == "229133" & days_on_supervision < 0 ~ 
        as.POSIXct("2025-10-02 06:00:00"),
      TRUE ~ supervision_start_date
    ),
    days_on_supervision = case_when(
      defendant_id_number %in% c("221409", "229133") & days_on_supervision < 0 ~ 
        as.numeric(
          difftime(
            if_else(
              is.na(disposition_date),
              as.Date(Sys.time()),
              as.Date(disposition_date)
            ),
            as.Date(supervision_start_date),
            units = "days"
          )
        ),
      TRUE ~ days_on_supervision
    )
  ) |> 
  # Drop 1 remaining obs. with negative days_on_supervision 
  filter(is.na(days_on_supervision) | days_on_supervision >= 0)

# Final check to ensure no weird data entries remain
View(clean_cases |> 
       filter(days_open < 0))



# ---------------------------------------------------------------------
# Add key indicator variables


# Create detained flag

clean_cases <- clean_cases |> 
  mutate(
    detained = case_when(
      is.na(release_date) ~ 1,
      TRUE ~ 0
  ))


# Create assigned level of supervision dummies

clean_cases <- clean_cases |>
  mutate(
    assigned_no_monitoring = as.integer(release_decision == "No Pretrial Monitoring"),
    assigned_level_1 = as.integer(
      release_decision %in% c(
        "Pretrial Monitoring Level 1, Passive",
        "Reminder Only"
      )
    ),
    assigned_level_2 = as.integer(release_decision == "Pretrial Monitoring Level 2, Active"),
    assigned_level_3 = as.integer(release_decision == "Pretrial Monitoring Level 3, Active")
  )


# Detained-level indicators
clean_cases <- clean_cases |>
  mutate(
    detained_no_monitoring = as.integer(detained == 1 & assigned_no_monitoring == 1),
    detained_level_1 = as.integer(detained == 1 & assigned_level_1 == 1),
    detained_level_2 = as.integer(detained == 1 & assigned_level_2 == 1),
    detained_level_3 = as.integer(detained == 1 & assigned_level_3 == 1)
  )



# Released-level indicators
clean_cases <- clean_cases |>
  mutate(
    release_no_monitoring = as.integer(detained == 0 & assigned_no_monitoring == 1),
    release_level_1 = as.integer(detained == 0 & assigned_level_1 == 1),
    release_level_2 = as.integer(detained == 0 & assigned_level_2 == 1),
    release_level_3 = as.integer(detained == 0 & assigned_level_3 == 1)
  )


# Create ROR (released on recognizance)

clean_cases <- clean_cases |>
  mutate(
    ror = case_when(
      bond_imposed == "FALSE" & !is.na(release_date) ~ 1,
      bond_imposed == "TRUE" & !is.na(bond_amount) & bond_amount == 0 ~ 1,
      TRUE ~ 0
    )
  )


# Create Sentenced indicator

clean_cases <- clean_cases |>
  mutate(
    sentenced = case_when(
      str_detect(disposition_reasons, "Sentenced") ~ 1,
      TRUE ~ 0
    )
  )


# Failure to appear to court pretrial

clean_cases <- clean_cases |>
  mutate(
    fta = case_when(
      str_detect(disposition_reasons, "Failure to Appear") ~ 1,
      TRUE ~ 0
    )
  )


# New arrest pretrial
clean_cases <- clean_cases |>
  mutate(
    new_arrest = case_when(
      str_detect(disposition_reasons, "Arrest") ~ 1,
      TRUE ~ 0
    )
  )



# Save cleaned data
fwrite(clean_cases, "Cleaned Data/cleaned_cases.csv")

