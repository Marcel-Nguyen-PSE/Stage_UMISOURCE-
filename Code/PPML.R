df_model_ppml <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    age = year - founded_date,
    log_age = log1p(pmax(age, 0)),

    # Past production and past visibility
    L2_log_publications = lag(log1p(n_publication), 2),
    L2_log_citations    = lag(log1p(n_citations), 2),

    # Lagged collaboration dummies
    L2_international = lag(as.integer(n_international_any > 0), 2),
    L2_inter_africa  = lag(as.integer(n_inter_african > 0), 2),
    L2_extra_africa  = lag(as.integer(n_extra_african > 0), 2)
  ) %>%
  ungroup() %>%
  mutate(
    country_year = interaction(country_code, year, drop = TRUE)
  )

# Citations PPML ---- 

c1 <- fepois(
  n_citations ~ ace_1 + ace_2 |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

c2 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

c3 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

c4 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations +
    L2_international |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

c5 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

c6 <- fepois(
  n_citations ~ ace_1 + ace_2 + deltas_1 +
    L2_log_publications +
    L2_log_citations +
    log_age +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model_ppml,
  cluster = ~ country_code
)

etable(
  c1, c2, c3, c4, c5, c6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    deltas_1 = "DELTAS I",
    L2_log_publications = "Log publications (t-2)",
    L2_log_citations = "Log citations (t-2)",
    log_age = "Log university age",
    L2_international = "International collaboration dummy (t-2)",
    L2_inter_africa = "Inter-African collaboration dummy (t-2)",
    L2_extra_africa = "Extra-African collaboration dummy (t-2)"
  ),
  fitstat = ~ n + pr2 + bic,
  se.below = TRUE,
  tex = TRUE,
  file = "Output/ppml_cit.tex"
)

# Publications PPML ----

m1 <- fepois(
  n_publication ~
    ace_1 + ace_2 |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

m2 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

m3 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_international |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

m4 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + year,
  data = df_model_ppml,
  cluster = ~ country_code
)

m5 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model_ppml,
  cluster = ~ country_code
)

m6 <- fepois(
  n_publication ~
    ace_1 + ace_2 + deltas_1 +
    L2_log_citations +
    log_age +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model_ppml,
  cluster = ~ country_code
)

etable(
  m1, m2, m3, m4, m5, m6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    deltas_1 = "DELTAS I",
    L2_log_citations = "Log citations (t-2)",
    log_cum_pub_lag = "Cumulative past publications",
    log_age = "Log university age",
    L2_international = "International collaboration dummy (t-2)",
    L2_inter_africa = "Inter-African collaboration dummy (t-2)",
    L2_extra_africa = "Extra-African collaboration dummy (t-2)"
  ),
  fitstat = ~ n + pr2 + bic,
  se.below = TRUE,
  tex = TRUE,
  file = "Output/ppml_pub.tex"
)

