df_ace12 <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  filter(
    ace1_ever == 1 | ace2_ever == 1,
    year >= 2009,
    year <= 2015,
    !is.na(n_citations),
    !is.na(n_publication),
    !is.na(country_code)
  ) %>%
  mutate(
    ace1_group = ace1_ever,
    post_ace1 = as.integer(year >= 2014),
    did_ace1 = ace1_group * post_ace1
  )

did_ppml_cit <- fepois(
  n_citations ~ did_ace1 |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

event_ppml_cit <- fepois(
  n_citations ~
    i(year, ace1_group, ref = 2013) |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

iplot(
  event_ppml_cit,
  main = "ACE I vs ACE II: PPML event-study, citations",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

did_ppml_pub <- fepois(
  n_publication ~ did_ace1 |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

event_ppml_pub <- fepois(
  n_publication ~
    i(year, ace1_group, ref = 2013) |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

iplot(
  event_ppml_pub,
  main = "ACE I vs ACE II: PPML event-study, publications",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

# Publications
jpeg(
  "Output/ACE1_PPML_event_study_publications.jpeg",
  width = 2400,
  height = 1800,
  res = 300,
  quality = 100
)

iplot(
  event_ppml_pub,
  main = "ACE I vs ACE II: PPML event-study (Publications)",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

dev.off()

# Citations
jpeg(
  "Output/ACE1_PPML_event_study_citations.jpeg",
  width = 2400,
  height = 1800,
  res = 300,
  quality = 100
)

iplot(
  event_ppml_cit,
  main = "ACE I vs ACE II: PPML event-study (Citations)",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

dev.off()
