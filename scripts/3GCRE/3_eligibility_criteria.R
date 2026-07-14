#=========================================================
# 04_create_patient_analysis_dataset.R
#
# Create patient-level analysis dataset
#=========================================================

#---------------------------------------------------------
# Setup
#---------------------------------------------------------

source(here::here("scripts", "00_setup.R"))

#---------------------------------------------------------
# Load data
#---------------------------------------------------------

combo_by_day_3gcre <- readRDS(
  here("data", "processed", "combo_by_day_3gcre.rds")
)

#---------------------------------------------------------
# Collapse to one record per patient
#---------------------------------------------------------

analysis_3gcre <- combo_by_day_3gcre %>%
  group_by(recordid) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    -day_relative_to_onset,
    -ab_combo,
    -appropriateness
  )

#---------------------------------------------------------
# Format analysis variables
#---------------------------------------------------------

analysis_3gcre <- analysis_3gcre %>%
  mutate(
    age_new = as.numeric(age_new),
    comorbidities_CCI = as.numeric(comorbidities_CCI),
    length_before_onset = as.numeric(length_before_onset)
  )

#---------------------------------------------------------
# Apply eligibility criteria
#---------------------------------------------------------

analysis_3gcre <- analysis_3gcre %>%
  filter(
    age_new >= 18,
    !(
      (!is.na(died_on_day) & died_on_day <= 1) |
        (!is.na(discharged_on_day) & discharged_on_day <= 1)
    )
  )

#---------------------------------------------------------
# Quality checks
#---------------------------------------------------------

cat(
  "Patients included in the analysis cohort:",
  n_distinct(analysis_3gcre$recordid),
  "\n"
)

cat(
  "Early appropriate therapy:",
  sum(analysis_3gcre$A_day0 == 1, na.rm = TRUE),
  "\n"
)

cat(
  "No early appropriate therapy:",
  sum(analysis_3gcre$A_day0 == 0, na.rm = TRUE),
  "\n"
)

# check
analysis_3gcre <- analysis_3gcre %>% 
  filter(length_before_onset >= 0 & length_before_onset < 350)
#---------------------------------------------------------
# Save dataset
#---------------------------------------------------------

saveRDS(
  analysis_3gcre,
  here("data", "processed", "analysis_3gcre.rds")
)

message("Patient-level analysis dataset successfully created.")
