#=========================================================
# Estimate marginal probability of treatment
#=========================================================

num_formula <- A_day0 ~ 1

m_num <- glm(
  formula = num_formula,
  data = analysis_3gcre,
  family = binomial()
)

analysis_3gcre$ps_num <- predict(
  m_num,
  type = "response"
)

#=========================================================
# Compute stabilized inverse probability weights
#=========================================================

w_raw <- ifelse(
  analysis_3gcre$A_day0 == 1,
  analysis_3gcre$ps_num / analysis_3gcre$ps,
  (1 - analysis_3gcre$ps_num) /
    (1 - analysis_3gcre$ps)
)

#=========================================================
# Truncate weights at the 1st and 99th percentiles
#=========================================================

q <- quantile(
  w_raw,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

analysis_3gcre <- analysis_3gcre %>%
  mutate(
    ipw = pmin(
      pmax(w_raw, q[1]),
      q[2]
    )
  )