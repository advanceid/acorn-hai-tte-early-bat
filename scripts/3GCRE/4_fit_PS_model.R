#=========================================================
# 04_fit_propensity_score_model.R
#
# Estimate propensity scores for early appropriate therapy
#=========================================================
#---------------------------------------------------------
# Prepare analysis dataset
#---------------------------------------------------------

analysis_3gcre <- analysis_3gcre %>%
  filter(
    !is.na(pitt_score), # complete case
    !is.na(admreason_bin),
  )
#---------------------------------------------------------
# Specify propensity score model
#---------------------------------------------------------

library(splines)
# Log length_before_onset
analysis_3gcre <- analysis_3gcre %>% 
  mutate(lbolog = log1p(length_before_onset))

# Propensity score model
ps_formula <- A_day0 ~
  pitt_score +
  severity_score +
  ICU_status_day0 +
  MV_status_day0 +
  adm_ward_types_new +
  ns(pre_ab_days, df = 3) +
  admtype_grp +
  admreason_bin +
  age_new +
  sex +
  country_income +
  lbolog +
  comorbidities_CCI +
  UTI_source +
  score_coagulation

#---------------------------------------------------------
# Predict propensity scores
#---------------------------------------------------------
m_ps <- glm(ps_formula, data = analysis_3gcre, family = binomial())
analysis_3gcre$ps <- predict(m_ps, type = "response")
#=========================================================
# Assess propensity score overlap
#=========================================================
ps_overlap <- ggplot(
  analysis_3gcre,
  aes(
    x = ps,
    fill = factor(A_day0)
  )
) +
  geom_density(
    alpha = 0.40
  ) +
  scale_fill_manual(
    values = c("#B2182B", "#2166AC"),
    labels = c(
      "No early appropriate therapy",
      "Early appropriate therapy"
    ),
    name = NULL
  ) +
  labs(
    x = "Propensity score",
    y = "Density"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )
ps_overlap