#=========================================================
# Add outcome information
#=========================================================
combo_by_day_3gcre <- combo_by_day_3gcre %>%
  left_join(
    baseline %>%
      select(
        recordid,
        first28_date,
        first28_death,
        mortality_date,
        ho_discharge_date,
        mortality
      ),
    by = "recordid"
  )
#=========================================================
# Define time-varying ICU and mechanical ventilation status
#=========================================================
# ICU admission/discharge variables
icu_adm_cols <- paste0("icu_hd_ap_", 1:5, "_2")
icu_dis_cols <- paste0("icu_hd_ap_", 1:5, "_3")

# Mechanical ventilation variables
mv_start_cols <- paste0("mv_ap_", 1:5, "_2")
mv_end_cols   <- paste0("mv_ap_", 1:5, "_3")

baseline <- baseline %>%
  mutate(
    across(
      all_of(c(
        icu_adm_cols,
        icu_dis_cols,
        mv_start_cols,
        mv_end_cols
      )),
      lubridate::ymd
    )
  )

in_interval_flag <- function(day, start_dates, end_dates){
  
  any(
    purrr::map2_lgl(
      start_dates,
      end_dates,
      ~{
        
        if(is.na(.x)) return(FALSE)
        
        if(is.na(.y)){
          day >= .x
        } else {
          day >= .x & day <= .y
        }
        
      }
    )
  )
  
}

combo_by_day_3gcre <- combo_by_day_3gcre %>%
  left_join(
    baseline %>%
      select(
        recordid,
        start_dates_icu,
        end_dates_icu,
        start_dates_mv,
        end_dates_mv
      ),
    by="recordid"
  ) %>%
  mutate(
    current_date =
      as.Date(inf_onset) + day_relative_to_onset
  ) %>%
  rowwise() %>%
  mutate(
    ICU_status =
      as.integer(
        in_interval_flag(
          current_date,
          start_dates_icu,
          end_dates_icu
        )
      ),
    MV_status =
      as.integer(
        in_interval_flag(
          current_date,
          start_dates_mv,
          end_dates_mv
        )
      )
  ) %>%
  ungroup() %>%
  mutate(
    ICU_status = replace_na(ICU_status,0L),
    MV_status = replace_na(MV_status,0L)
  )

#=========================================================
# Define mortality outcomes
#=========================================================

combo_by_day_3gcre <- combo_by_day_3gcre %>%
  mutate(
    discharged_on_day =
      as.integer(ho_discharge_date - inf_onset),
    
    died_on_day =
      as.integer(mortality_date - inf_onset),
    
    seven_day_death =
      if_else(
        !is.na(died_on_day) & died_on_day <= 7,
        1L,
        0L
      ),
    
    fourteen_day_death =
      if_else(
        !is.na(died_on_day) & died_on_day <= 14,
        1L,
        0L
      ),
    
    in_hospital_mortality =
      if_else(
        mortality == 1,
        1L,
        0L
      )
  )
#=========================================================
# Define antibiotic exposure before infection onset
#=========================================================

combo_by_day_3gcre <- combo_by_day_3gcre %>%
  group_by(recordid,inf_onset) %>%
  mutate(
    pre_ab_days =
      n_distinct(
        current_date[
          day_relative_to_onset<0 &
            !is.na(ab_combo) &
            ab_combo!=""
        ]
      )
  ) %>%
  ungroup() %>%
  mutate(
    pre_ab_cat =
      case_when(
        pre_ab_days==0 ~ "0 days",
        between(pre_ab_days,1,7) ~ "1-7 days",
        pre_ab_days>7 ~ ">7 days"
      ),
    pre_ab_cat =
      factor(
        pre_ab_cat,
        levels=c(
          "0 days",
          "1-7 days",
          ">7 days"
        )
      )
  )

#=========================================================
# Add baseline covariates
#=========================================================

combo_by_day_3gcre <- combo_by_day_3gcre %>%
  left_join(
    baseline %>%
      select(
        recordid,
        age_new,
        sex,
        country,
        country_income,
        country_region,
        adm_ward_types_new,
        hpd_admreason,
        hpd_admreason,
        hpd_admtype,
        comorbidities_CCI,
        sofa_score,
        severity_score,
        pitt_score,
        length_before_onset,
        UTI_source, score_coagulation, score_liver, score_CVS, score_CNV, score_renal
      ),
    by="recordid"
  )

#=========================================================
# Define ICU and mechanical ventilation status
# at infection onset
#=========================================================
combo_by_day_3gcre <- combo_by_day_3gcre %>%
  group_by(recordid) %>%
  mutate(
    ICU_status_day0 =
      as.integer(
        any(
          day_relative_to_onset %in% -1:1 &
            ICU_status==1
        )
      ),
    MV_status_day0 =
      as.integer(
        any(
          day_relative_to_onset %in% -1:1 &
            MV_status==1
        )
      )
  ) %>%
  ungroup()

#=========================================================
# Define admission characteristics
#=========================================================
# riclassifica
norm <- function(x) toupper(trimws(as.character(x)))

combo_by_day_3gcre <- combo_by_day_3gcre %>%
  mutate(
    ward_raw = norm(hpd_admreason),
    type_raw = norm(hpd_admtype),
    admtype_grp = case_when(
      ward_raw == "INF" ~ "INFECTIOUS",
      ward_raw %in% c("HMD","ONC") ~ "HEMATO_ONC",
      ward_raw %in% c("ORT","TRA","GYN","GUD") ~ "SURGICAL",
      ward_raw %in% c("CARD","PMD","REN","GIT","NRD","EMD","DRM","CTD") ~ "MEDICAL",
      TRUE ~ "OTHER"   # cattura tutto il resto, inclusi NA
    ),
    admreason_bin = case_when(
      type_raw == "EMR" ~ "EMR",
      type_raw == "ELT" ~ "ELT",
      TRUE ~ NA_character_
    ),
    admtype_grp   = factor(admtype_grp,
                           levels = c("MEDICAL","INFECTIOUS","HEMATO_ONC","SURGICAL","OTHER")),
    admreason_bin = factor(admreason_bin, levels = c("ELT","EMR"))
  )

saveRDS(
  combo_by_day_3gcre,
  here("data", "processed", "combo_by_day_3gcre.rds")
)
