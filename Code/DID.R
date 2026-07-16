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

# Propensity-score matching DID estimate ---- 

df_plot_metrics <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    treatment_group = case_when(
      any(ace_1 == 1, na.rm = TRUE) ~ "ACE I",
      any(ace_2 == 1, na.rm = TRUE) ~ "ACE II",
      TRUE ~ "Never treated"
    )
  ) %>%
  ungroup() %>%
  mutate(
    publications = log1p(n_publication),
    citations = log1p(n_citations),
    international_collaboration = international_share_any
  ) %>%
  select(
    inst_id,
    year,
    treatment_group,
    publications,
    citations,
    international_collaboration
  ) %>%
  pivot_longer(
    cols = c(publications, citations, international_collaboration),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(year, treatment_group, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  ) %>%
  group_by(treatment_group, metric) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    baseline = mean_value[year == 2006][1],
    mean_value = mean_value - baseline,
    ymin = mean_value - se,
    ymax = mean_value + se
  ) %>%
  ungroup() %>%
  mutate(
    metric = case_when(
      metric == "publications" ~ "Publications",
      metric == "citations" ~ "Citations",
      metric == "international_collaboration" ~ "International collaboration",
      TRUE ~ metric
    ),
    treatment_group = factor(
      treatment_group,
      levels = c("Never treated", "ACE I", "ACE II")
    )
  )

p_metrics <- ggplot(
  data = df_plot_metrics,
  aes(
    x = year,
    y = mean_value,
    colour = treatment_group,
    fill = treatment_group
  )
) +
  geom_line(linewidth = 1.25) +
  geom_vline(
    data = distinct(df_plot_metrics, metric),
    aes(xintercept = 2014),
    inherit.aes = FALSE,
    linetype = "dashed",
    colour = "black"
  ) +
  geom_vline(
    data = distinct(df_plot_metrics, metric),
    aes(xintercept = 2016),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "black"
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    nrow = 1
  ) +
  scale_colour_manual(
    values = c(
      "Never treated" = "#D55E00",
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Never treated" = "#D55E00",
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  labs(
    x = "Year",
    y = "Change relative to 2006 average",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave('Output/p_metrics.jpeg', p_metrics, width = 16, height = 9, dpi = 500)

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treated_ever = as.integer(ace1_ever == 1 | ace2_ever == 1)
  ) %>%
  ungroup()

pre_chars <- df_base %>%
  filter(year <= 2013) %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    intl_dummy = as.integer(n_international_any > 0)
  ) %>%
  group_by(inst_id) %>%
  summarise(
    pub_2013 = log_pub[year == 2013][1],
    cit_2013 = log_cit[year == 2013][1],

    pub_slope_pre = if (sum(!is.na(log_pub)) >= 3) {
      coef(lm(log_pub ~ year))[2]
    } else NA_real_,

    cit_slope_pre = if (sum(!is.na(log_cit)) >= 3) {
      coef(lm(log_cit ~ year))[2]
    } else NA_real_,

    intl_pre = mean(intl_dummy, na.rm = TRUE),

    .groups = "drop"
  )

match_data <- df_base %>%
  filter(year == 2013) %>%
  select(
    inst_id,
    name,
    country_code,
    founded_date,
    treated_ever
  ) %>%
  left_join(pre_chars, by = "inst_id") %>%
  mutate(
    log_age_2013 = log1p(pmax(2013 - founded_date, 0))
  ) %>%
  select(
    inst_id,
    name,
    country_code,
    treated_ever,
    pub_2013,
    pub_slope_pre,
    cit_2013,
    cit_slope_pre,
    intl_pre,
    log_age_2013
  ) %>%
  drop_na()

table(match_data$treated_ever)

ps_match <- matchit(
  treated_ever ~
    pub_2013 +
    pub_slope_pre +
    cit_2013 +
    cit_slope_pre +
    intl_pre +
    log_age_2013,
  data = match_data,
  method = "nearest",
  distance = "logit",
  ratio = 3,
  replace = FALSE
)

# Love plot ----

love.plot(
  ps_match,
  stats = "mean.diffs",
  abs = TRUE,
  threshold = 0.1,
  var.order = "unadjusted",
  binary = "std",
  stars = "raw",
  colors = c("grey55", "#0072B2"),
  shapes = c(16, 17),
  size = 3
) +
  labs(
    x = "Absolute standardized mean difference",
    y = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "Output/covariate_balance_loveplot.jpeg",
  width = 9,
  height = 6,
  dpi = 300
)

matched_units <- match.data(ps_match) %>%
  select(inst_id, treated_ever, weights)

df_matched <- df_base %>%
  inner_join(matched_units, by = c("inst_id", "treated_ever")) %>%
  mutate(
    y_publications = n_publication,
    y_citations = n_citations,
    y_international = international_share_any
  )

run_event_pois <- function(data, outcome, treat_var, ref_year) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(year, ", treat_var, ", ref = ", ref_year, ") | inst_id + year"
    )
  )

  fepois(
    fml,
    data = data,
    weights = ~ weights,
    cluster = ~ country_code
  )
}

run_event_ols <- function(data, outcome, treat_var, ref_year) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(year, ", treat_var, ", ref = ", ref_year, ") | inst_id + year"
    )
  )

  feols(
    fml,
    data = data,
    weights = ~ weights,
    cluster = ~ country_code
  )
}

models <- list(
  pub_ace1 = run_event_pois(
    df_matched %>%
      filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_publications)),
    "y_publications",
    "ace1_ever",
    2013
  ),

  pub_ace2 = run_event_pois(
    df_matched %>%
      filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_publications)),
    "y_publications",
    "ace2_ever",
    2015
  ),

  cit_ace1 = run_event_pois(
    df_matched %>%
      filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_citations)),
    "y_citations",
    "ace1_ever",
    2013
  ),

  cit_ace2 = run_event_pois(
    df_matched %>%
      filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_citations)),
    "y_citations",
    "ace2_ever",
    2015
  ),

  intl_ace1 = run_event_ols(
    df_matched %>%
      filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_international)),
    "y_international",
    "ace1_ever",
    2013
  ),

  intl_ace2 = run_event_ols(
    df_matched %>%
      filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_international)),
    "y_international",
    "ace2_ever",
    2015
  )
)

extract_event <- function(model, outcome_label, program_label, treat_var, ref_year, treat_year) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl(paste0("year::[0-9]+:", treat_var), term)) %>%
    mutate(
      year = as.integer(sub("year::([0-9]+):.*", "\\1", term)),
      outcome = outcome_label,
      program = program_label
    ) %>%
    select(outcome, program, year, estimate, conf.low, conf.high) %>%
    bind_rows(
      tibble(
        outcome = outcome_label,
        program = program_label,
        year = ref_year,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      )
    ) %>%
    mutate(
      event_time = year - treat_year
    )
}

event_df <- bind_rows(
  extract_event(models$pub_ace1,  "Publications", "ACE I",  "ace1_ever", 2013, 2014),
  extract_event(models$pub_ace2,  "Publications", "ACE II", "ace2_ever", 2015, 2016),
  extract_event(models$cit_ace1,  "Citations", "ACE I",  "ace1_ever", 2013, 2014),
  extract_event(models$cit_ace2,  "Citations", "ACE II", "ace2_ever", 2015, 2016),
  extract_event(models$intl_ace1, "International collaboration", "ACE I",  "ace1_ever", 2013, 2014),
  extract_event(models$intl_ace2, "International collaboration", "ACE II", "ace2_ever", 2015, 2016)
) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Publications", "Citations", "International collaboration")
    ),
    program = factor(program, levels = c("ACE I", "ACE II"))
  )

# DID plotting code ----

jpeg(
  filename = "Output/pub_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  units = "px",
  res = 300,
  quality = 100
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 4, 2) + 0.1,
  oma = c(0, 0, 3, 0)
)

iplot(
  models$pub_ace1,
  main = "ACE I",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

iplot(
  models$pub_ace2,
  main = "ACE II",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

mtext(
  "Publications",
  outer = TRUE,
  side = 3,
  line = 1,
  font = 2,
  cex = 1.4
)

dev.off()

jpeg(
  filename = "Output/cit_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  units = "px",
  res = 300,
  quality = 100
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 4, 2) + 0.1,
  oma = c(0, 0, 3, 0)
)

iplot(
  models$cit_ace1,
  main = "ACE I",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

iplot(
  models$cit_ace2,
  main = "ACE II",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

mtext(
  "Citations",
  outer = TRUE,
  side = 3,
  line = 1,
  font = 2,
  cex = 1.4
)

dev.off()

jpeg(
  filename = "Output/int_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  units = "px",
  res = 300,
  quality = 100
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 4, 2) + 0.1,
  oma = c(0, 0, 3, 0)
)

iplot(
  models$intl_ace1,
  main = "ACE I",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

iplot(
  models$intl_ace2,
  main = "ACE II",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

mtext(
  "International Collaboration",
  outer = TRUE,
  side = 3,
  line = 1,
  font = 2,
  cex = 1.4
)

dev.off()

