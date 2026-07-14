#=========================================================
# 03_create_patient_analysis_dataset.R
#
# Create patient-level analysis dataset
#=========================================================
#---------------------------------------------------------
# Collapse to one record per patient
#---------------------------------------------------------

analysis_cre <- combo_by_day_cre %>%
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

analysis_cre <- analysis_cre %>%
  mutate(
    age_new = as.numeric(age_new),
    comorbidities_CCI = as.numeric(comorbidities_CCI),
    length_before_onset = as.numeric(length_before_onset)
  )

#---------------------------------------------------------
# Apply eligibility criteria
#---------------------------------------------------------

analysis_cre <- analysis_cre %>%
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
  n_distinct(analysis_cre$recordid),
  "\n"
)
cat(
  "Early appropriate therapy:",
  sum(analysis_cre$A_day0 == 1, na.rm = TRUE),
  "\n"
)
cat(
  "No early appropriate therapy:",
  sum(analysis_cre$A_day0 == 0, na.rm = TRUE),
  "\n"
)