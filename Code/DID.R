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

