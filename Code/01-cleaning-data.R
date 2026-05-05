library(readr)
library(janitor)
library(tidyverse)
library(data.table)

# ---------------------------------------------------------------------
# EXPLORING AND CLEANING PRETRIAL DATA

all_cases <- read_csv("Data/All_Cases.csv") |> 
  # Change all variable names to snake_case
  clean_names() |> 
  relocate(judicial_override, .after = "supervision_level") |> 
  # Rename incorrectly named variable
  rename(case_number = cause_number) |> 
  # Drop alternative defendant id
  select(-defendant_id) 


# View distribution of variables
skimr::skim(all_cases)


clean_cases <- all_cases |> 
  # Drop columns with near-zero variance or all missing values 
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
  # Drop counties that failed to correctly implement program (per pretrial program manager)
  filter(
    !caseload_name %in% c("Central Office", "Ravalli", "Broadwater County")
  ) |> 
  # Drop cases that never used the PSA and could be faulty (per case manager)
  filter(!str_detect(case_number, "PC") & 
           !recommended_release_decision == "None") |> 
  # Drop cases marked to be deleted while not dropping ones with missing arrest_number
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


# View distribution of variables post initial cleaning
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
stopifnot(sum(clean_cases$days_on_supervision < 0, na.rm = TRUE) == 0)


# Investigate DOB variable
View(clean_cases |> 
       arrange((date_of_birth)))

clean_cases <- clean_cases |> 
  mutate(
    # Fix wrong DOB after checking original PSA report from case manager
    date_of_birth = if_else(
      date_of_birth == as.POSIXct("1875-06-03 06:59:56"),
      as.POSIXct("1900-01-01 07:00:00"),
      date_of_birth
    ),
    # Create age (at arrest) variable
    age = floor(time_length(interval(as.Date(date_of_birth), as.Date(arrest_date)), "years"))
  ) |> 
  # Restrict to adult cases for IRB compliance
  filter(age >= 18) |> 
  select(
    # Drop DOB since it is a direct identifier
    -date_of_birth
  )


# ---------------------------------------------------------------------
# ADD KEY INDICATOR VARIABLES

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

# Indicators for whether judge assigned above or below recommended level of supervision
clean_cases <- clean_cases |>
  mutate(
    assigned_level_num = case_when(
      release_decision == "No Pretrial Monitoring" ~ 0,
      release_decision %in% c("Pretrial Monitoring Level 1, Passive", "Reminder Only") ~ 1,
      release_decision == "Pretrial Monitoring Level 2, Active" ~ 2,
      release_decision == "Pretrial Monitoring Level 3, Active" ~ 3,  
    ),
    recommended_level_num = case_when(
      recommended_release_decision == "No Pretrial Monitoring" ~ 0,
      recommended_release_decision %in% c("Pretrial Monitoring Level 1, Passive", "Reminder Only") ~ 1,
      recommended_release_decision == "Pretrial Monitoring Level 2, Active" ~ 2,
      recommended_release_decision == "Pretrial Monitoring Level 3, Active" ~ 3,
    ),
    assigned_above_recommended = as.integer(assigned_level_num > recommended_level_num),
    assigned_below_recommended = as.integer(assigned_level_num < recommended_level_num),
    assigned_equals_recommended = as.integer(assigned_level_num == recommended_level_num)
  )


# Create ROR (Released on Recognizance)

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


# Failure to Appear to court pretrial

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

# Offense Type and Count

extract_offense_code <- function(x) {
  code <- str_extract(x, "^\\s*\\d{1,2}-\\d{1,2}-\\d{2,4}")
  str_remove(code, "^\\s*")
}

extract_offense_count <- function(x) {
  x_low <- str_to_lower(x)
  
  case_when(
    str_detect(x_low, "\\[\\s*1st\\s*\\]") | str_detect(x_low, "1st\\s+offense") ~ "1st",
    str_detect(x_low, "\\[\\s*2nd\\s*\\]") | str_detect(x_low, "2nd\\s+offense") ~ "2nd",
    str_detect(x_low, "\\[\\s*3rd\\+\\s*\\]") | str_detect(x_low, "3rd\\s*\\+|3rd\\s+offense") ~ "3rd_plus",
    str_detect(x_low, "\\[\\s*4th\\+\\s*\\]") | str_detect(x_low, "4th\\s*\\+|4th\\s+or\\s+subsequent") ~ "4th_plus",
    TRUE ~ NA_character_
  )
}


clean_cases <- clean_cases |>
  mutate(
    most_serious_offense = as.character(most_serious_offense),
    # ---------------------------------------------------------------------
    # Offense classifications
    offense_code = extract_offense_code(most_serious_offense),
    offense_title = if_else(
      !is.na(offense_code),
      str_extract(offense_code, "^\\d{1,2}-\\d{1,2}"),
      NA_character_
    ),
    offense_bucket = case_when(
      offense_title == "45-5" ~ "person_violent",
      offense_title == "45-6" ~ "property",
      offense_title %in% c("45-9", "45-10") ~ "drug",
      str_detect(offense_code, "^61-") ~ "traffic",
      offense_title %in% c("45-7", "45-8") | str_detect(offense_code, "^(3-|46-23)") ~ "public_order",
      str_detect(offense_code, "^52-") ~ "person_violent",
      TRUE ~ "other"
    ),
    # Indicator variables 
    person_violent_charge = as.integer(offense_bucket == "person_violent"),
    property_charge = as.integer(offense_bucket == "property"),
    drug_charge = as.integer(offense_bucket == "drug"),
    traffic_charge = as.integer(offense_bucket == "traffic"),
    public_order_charge = as.integer(offense_bucket == "public_order"),
    other_charge = as.integer(offense_bucket == "other"),
    # Specific offense flags for Partner Family Member Assault crimes
    # This may be particularly sensitive to getting harsher pretrial assignments
    pfma_charge = as.integer(
      str_detect(
        str_to_lower(most_serious_offense),
        "pfma|partner\\s+or\\s+family\\s+member\\s+assault|partner family member assault")),
    # ---------------------------------------------------------------------
    # Offense count 
    offense_count_raw = extract_offense_count(most_serious_offense),
    # If offense has no count variable in description, treat as "unknown"
    offense_count = if_else(is.na(offense_count_raw), "unknown", offense_count_raw),
    offense_count = factor(offense_count, levels = c("unknown","1st","2nd","3rd_plus","4th_plus")),
    # Dummy variables for counts
    offense_count_unknown  = as.integer(offense_count == "unknown"),
    first_offense        = as.integer(offense_count == "1st"),
    second_offense       = as.integer(offense_count == "2nd"),
    third_plus_offense   = as.integer(offense_count == "3rd_plus"),
    fourth_plus_offense  = as.integer(offense_count == "4th_plus")
  ) |>
  select(-offense_count_raw,
         -offense_title)

# Quick checks 
clean_cases |>
  count(offense_bucket, sort = TRUE)
clean_cases |>
  count(offense_count, sort = TRUE)

# Check within PFMA 
clean_cases |> filter(pfma_charge == 1) |> count(offense_count, sort = TRUE)


# Save cleaned data
fwrite(clean_cases, "Cleaned Data/cleaned_cases.csv")
