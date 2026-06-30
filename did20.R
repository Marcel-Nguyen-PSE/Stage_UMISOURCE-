library(dplyr)
library(fixest)

# ============================================================
# 1. Build ACE I vs ACE II sample
#    ACE II is used only as not-yet-treated control
#    Therefore: stop in 2015
# ============================================================

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
    !is.na(n_publication),
    !is.na(country_code)
  ) %>%
  mutate(
    ace1_group = ace1_ever,
    post_ace1 = as.integer(year >= 2014),
    did_ace1 = ace1_group * post_ace1
  )

# Check sample
df_ace12 %>%
  distinct(inst_id, name, country_code, ace1_ever, ace2_ever) %>%
  count(ace1_ever, ace2_ever)

# ============================================================
# 2. Main DID: Poisson FE
# ============================================================

did_ppml_ace1 <- fepois(
  n_publication ~ did_ace1 |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

summary(did_ppml_ace1)

# Percentage effect
100 * (exp(coef(did_ppml_ace1)["did_ace1"]) - 1)

# ============================================================
# 3. Event-study / pre-trend check
# ============================================================

event_ppml_ace1 <- fepois(
  n_publication ~
    i(year, ace1_group, ref = 2013) |
    inst_id + year,
  data = df_ace12,
  cluster = ~ country_code
)

summary(event_ppml_ace1)

iplot(
  event_ppml_ace1,
  main = "ACE I vs ACE II: PPML event-study",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 4. Joint pre-trend test
# ============================================================

pre_coef <- names(coef(event_ppml_ace1))[
  grepl("year::20(09|10|11|12):ace1_group", names(coef(event_ppml_ace1)))
]

wald(
  event_ppml_ace1,
  paste0(pre_coef, " = 0")
)








library(dplyr)
library(fixest)

# ============================================================
# 1. ACE I vs ACE II sample
#    ACE II is used only as not-yet-treated control
#    Therefore: stop in 2015
# ============================================================

df_ace12_cit <- df_africa %>%
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
    !is.na(country_code)
  ) %>%
  mutate(
    ace1_group = ace1_ever,
    post_ace1 = as.integer(year >= 2014),
    did_ace1 = ace1_group * post_ace1
  )

# Check sample
df_ace12_cit %>%
  distinct(inst_id, name, country_code, ace1_ever, ace2_ever) %>%
  count(ace1_ever, ace2_ever)

# ============================================================
# 2. Main DID: Poisson FE for citations
# ============================================================

did_ppml_cit <- fepois(
  n_citations ~ did_ace1 |
    inst_id + year,
  data = df_ace12_cit,
  cluster = ~ country_code
)

summary(did_ppml_cit)

# Percentage effect
100 * (exp(coef(did_ppml_cit)["did_ace1"]) - 1)

# ============================================================
# 3. Event-study / pre-trend check
# ============================================================

event_ppml_cit <- fepois(
  n_citations ~
    i(year, ace1_group, ref = 2013) |
    inst_id + year,
  data = df_ace12_cit,
  cluster = ~ country_code
)

summary(event_ppml_cit)

iplot(
  event_ppml_cit,
  main = "ACE I vs ACE II: PPML event-study, citations",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 4. Joint pre-trend test
# ============================================================

pre_coef_cit <- names(coef(event_ppml_cit))[
  grepl("year::20(09|10|11|12):ace1_group", names(coef(event_ppml_cit)))
]

wald(
  event_ppml_cit,
  paste0(pre_coef_cit, " = 0")
)



# Publications
jpeg(
  "ACE1_PPML_event_study_publications.jpeg",
  width = 2400,
  height = 1800,
  res = 300,
  quality = 100
)

iplot(
  event_ppml_ace1,
  main = "ACE I vs ACE II: PPML event-study (Publications)",
  xlab = "Year",
  ylab = "Log incidence-rate ratio",
  ref.line = 0,
  ci_level = 0.95
)

dev.off()


# Citations
jpeg(
  "ACE1_PPML_event_study_citations.jpeg",
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







library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)
library(ggplot2)
library(broom)

# ============================================================
# 1. Build treatment indicators
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treated_ever = as.integer(ace1_ever == 1 | ace2_ever == 1),
    treat_year = case_when(
      ace1_ever == 1 ~ 2014,
      ace2_ever == 1 ~ 2016,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ============================================================
# 2. Pre-treatment variables for propensity-score matching
#    No macro controls with heavy missingness
# ============================================================

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
    } else {
      NA_real_
    },

    cit_slope_pre = if (sum(!is.na(log_cit)) >= 3) {
      coef(lm(log_cit ~ year))[2]
    } else {
      NA_real_
    },

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

# ============================================================
# 3. Propensity-score matching
# ============================================================

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

summary(ps_match)
love.plot(ps_match, threshold = 0.1)

matched_units <- match.data(ps_match) %>%
  select(inst_id, treated_ever, weights, subclass)

# ============================================================
# 4. Build matched panel
# ============================================================

df_matched <- df_base %>%
  inner_join(matched_units, by = c("inst_id", "treated_ever")) %>%
  filter(!is.na(n_publication)) %>%
  mutate(
    group = case_when(
      ace1_ever == 1 ~ "ACE I",
      ace2_ever == 1 ~ "ACE II",
      TRUE ~ "Matched controls"
    ),
    event_time = case_when(
      ace1_ever == 1 ~ year - 2014,
      ace2_ever == 1 ~ year - 2016,
      TRUE ~ -1000
    ),
    country_year = interaction(country_code, year, drop = TRUE)
  )

# ============================================================
# 5. Matched DID event-study for ACE I
# ============================================================

event_ace1_matched <- feols(
  log1p(n_publication) ~
    i(year, ace1_ever, ref = 2013) |
    inst_id + year,
  data = df_matched %>%
    filter(ace1_ever == 1 | treated_ever == 0),
  weights = ~weights,
  cluster = ~country_code
)

# ============================================================
# 6. Matched DID event-study for ACE II
# ============================================================

event_ace2_matched <- feols(
  log1p(n_publication) ~
    i(year, ace2_ever, ref = 2015) |
    inst_id + year,
  data = df_matched %>%
    filter(ace2_ever == 1 | treated_ever == 0),
  weights = ~weights,
  cluster = ~country_code
)

summary(event_ace1_matched)
summary(event_ace2_matched)

# ============================================================
# 7. Extract event-study coefficients
# ============================================================

extract_event <- function(model, program_name, treat_var) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl(paste0("year::"), term)) %>%
    mutate(
      year = as.integer(gsub(
        paste0("year::([0-9]+):", treat_var),
        "\\1",
        term
      )),
      program = program_name
    ) %>%
    select(program, year, estimate, conf.low, conf.high)
}

event_df <- bind_rows(
  extract_event(event_ace1_matched, "ACE I", "ace1_ever"),
  extract_event(event_ace2_matched, "ACE II", "ace2_ever")
)

# Add reference years manually
event_df <- bind_rows(
  event_df,
  tibble(
    program = c("ACE I", "ACE II"),
    year = c(2013, 2015),
    estimate = 0,
    conf.low = 0,
    conf.high = 0
  )
) %>%
  mutate(
    event_time = case_when(
      program == "ACE I" ~ year - 2014,
      program == "ACE II" ~ year - 2016
    )
  )

# ============================================================
# 8. Combined plot: ACE I and ACE II treatment curves
# ============================================================

ggplot(
  event_df,
  aes(
    x = event_time,
    y = estimate,
    colour = program,
    fill = program
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "black"
  ) +
  geom_vline(
    xintercept = -1,
    linetype = "dashed",
    colour = "black"
  ) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_colour_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  scale_fill_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  labs(
    x = "Years relative to treatment",
    y = "Effect on log(publications + 1)",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )





library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)
library(broom)
library(ggplot2)

# ============================================================
# 1. Treatment indicators
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treated_ever = as.integer(ace1_ever == 1 | ace2_ever == 1)
  ) %>%
  ungroup()

# ============================================================
# 2. Pre-treatment variables for propensity-score matching
#    No macro controls
# ============================================================

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
  select(inst_id, name, country_code, founded_date, treated_ever) %>%
  left_join(pre_chars, by = "inst_id") %>%
  mutate(
    log_age_2013 = log1p(pmax(2013 - founded_date, 0))
  ) %>%
  select(
    inst_id, name, country_code, treated_ever,
    pub_2013, pub_slope_pre,
    cit_2013, cit_slope_pre,
    intl_pre, log_age_2013
  ) %>%
  drop_na()

table(match_data$treated_ever)

# ============================================================
# 3. Propensity-score matching
# ============================================================

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

summary(ps_match)
love.plot(ps_match, threshold = 0.1)

matched_units <- match.data(ps_match) %>%
  select(inst_id, treated_ever, weights)

# ============================================================
# 4. Matched panel
# ============================================================

df_matched <- df_base %>%
  inner_join(matched_units, by = c("inst_id", "treated_ever")) %>%
  mutate(
    y_publications = log1p(n_publication),
    y_citations = log1p(n_citations),
    y_international = international_share_any
  )

# ============================================================
# 5. Function: estimate event-study
# ============================================================

run_event <- function(data, outcome, program, treat_var, ref_year) {

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

# ============================================================
# 6. ACE I and ACE II event-studies by outcome
# ============================================================

models <- list(
  pub_ace1 = run_event(
    df_matched %>% filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_publications)),
    "y_publications", "ACE I", "ace1_ever", 2013
  ),

  pub_ace2 = run_event(
    df_matched %>% filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_publications)),
    "y_publications", "ACE II", "ace2_ever", 2015
  ),

  cit_ace1 = run_event(
    df_matched %>% filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_citations)),
    "y_citations", "ACE I", "ace1_ever", 2013
  ),

  cit_ace2 = run_event(
    df_matched %>% filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_citations)),
    "y_citations", "ACE II", "ace2_ever", 2015
  ),

  intl_ace1 = run_event(
    df_matched %>% filter(ace1_ever == 1 | treated_ever == 0, !is.na(y_international)),
    "y_international", "ACE I", "ace1_ever", 2013
  ),

  intl_ace2 = run_event(
    df_matched %>% filter(ace2_ever == 1 | treated_ever == 0, !is.na(y_international)),
    "y_international", "ACE II", "ace2_ever", 2015
  )
)

# ============================================================
# 7. Extract coefficients
# ============================================================

extract_event <- function(model, outcome_label, program_label, treat_var, ref_year, treat_year) {

  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl(paste0(":?", treat_var), term)) %>%
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

# ============================================================
# 8. One-page DID plot
# ============================================================

p_did_all <- ggplot(
  event_df,
  aes(
    x = event_time,
    y = estimate,
    colour = program,
    fill = program
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "black") +
  geom_vline(xintercept = -1, linetype = "dashed", colour = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 3) +
  scale_colour_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  scale_fill_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  labs(
    x = "Years relative to treatment",
    y = "DID estimate",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_did_all

# ============================================================
# 9. Optional export
# ============================================================

ggsave(
  "matched_did_ace1_ace2_three_outcomes.jpeg",
  p_did_all,
  width = 10,
  height = 12,
  dpi = 300
)






library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)
library(broom)
library(ggplot2)

# ============================================================
# 1. Treatment indicators
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treated_ever = as.integer(ace1_ever == 1 | ace2_ever == 1)
  ) %>%
  ungroup()

# ============================================================
# 2. Pre-treatment variables for propensity-score matching
#    No macro controls with high missingness
# ============================================================

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

# ============================================================
# 3. Propensity-score matching
# ============================================================

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

summary(ps_match)
love.plot(ps_match, threshold = 0.1)

matched_units <- match.data(ps_match) %>%
  select(inst_id, treated_ever, weights)

# ============================================================
# 4. Matched panel
# ============================================================

df_matched <- df_base %>%
  inner_join(matched_units, by = c("inst_id", "treated_ever")) %>%
  mutate(
    y_publications = n_publication,
    y_citations = n_citations,
    y_international = international_share_any
  )

# ============================================================
# 5. Event-study functions
#    fepois for counts, feols for collaboration share
# ============================================================

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

# ============================================================
# 6. Estimate ACE I and ACE II event-studies by outcome
# ============================================================

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

summary(models$pub_ace1)
summary(models$pub_ace2)
summary(models$cit_ace1)
summary(models$cit_ace2)
summary(models$intl_ace1)
summary(models$intl_ace2)

# ============================================================
# 7. Extract coefficients
# ============================================================

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

# ============================================================
# 8. One-page DID plot
# ============================================================

p_did_all <- ggplot(
  event_df,
  aes(
    x = event_time,
    y = estimate,
    colour = program,
    fill = program
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "black") +
  geom_vline(xintercept = -1, linetype = "dashed", colour = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 3) +
  scale_colour_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  scale_fill_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  labs(
    x = "Years relative to treatment",
    y = "DID estimate",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_did_all

# ============================================================
# 9. Optional export
# ============================================================

ggsave(
  "matched_did_ppml_feols_ace1_ace2_three_outcomes.jpeg",
  p_did_all,
  width = 10,
  height = 12,
  dpi = 300
)






library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)
library(broom)
library(ggplot2)

# ============================================================
# 1. Treatment indicators
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treated_ever = as.integer(ace1_ever == 1 | ace2_ever == 1)
  ) %>%
  ungroup()

# ============================================================
# 2. Pre-treatment variables for propensity-score matching
#    No macro controls with high missingness
# ============================================================

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

# ============================================================
# 3. Propensity-score matching
# ============================================================

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

summary(ps_match)
love.plot(ps_match, threshold = 0.1)

matched_units <- match.data(ps_match) %>%
  select(inst_id, treated_ever, weights)

# ============================================================
# 4. Matched panel
# ============================================================

df_matched <- df_base %>%
  inner_join(matched_units, by = c("inst_id", "treated_ever")) %>%
  mutate(
    y_publications = n_publication,
    y_citations = n_citations,
    y_international = international_share_any
  )

# ============================================================
# 5. Event-study functions
#    fepois for counts, feols for collaboration share
# ============================================================

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

# ============================================================
# 6. Estimate ACE I and ACE II event-studies by outcome
# ============================================================

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

summary(models$pub_ace1)
summary(models$pub_ace2)
summary(models$cit_ace1)
summary(models$cit_ace2)
summary(models$intl_ace1)
summary(models$intl_ace2)

# ============================================================
# 7. Extract coefficients
# ============================================================

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

# ============================================================
# 8. One-page DID plot
# ============================================================

p_did_all <- ggplot(
  event_df,
  aes(
    x = event_time,
    y = estimate,
    colour = program,
    fill = program
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "black") +
  geom_vline(xintercept = -1, linetype = "dashed", colour = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 3) +
  scale_colour_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  scale_fill_manual(
    values = c(
      "ACE I" = "#0072B2",
      "ACE II" = "#009E73"
    )
  ) +
  labs(
    x = "Years relative to treatment",
    y = "DID estimate",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_did_all

# ============================================================
# 9. Optional export
# ============================================================

ggsave(
  "matched_did_ppml_feols_ace1_ace2_three_outcomes.jpeg",
  p_did_all,
  width = 10,
  height = 12,
  dpi = 300
)



library(patchwork)

#=========================================================
# Publications
#=========================================================

jpeg('pub_ace1_ace2_did.jpeg', width = 16, height = 9, res = 500, quality = 100)

par(

  mfrow = c(1, 2),

  mar = c(4, 4, 3, 1),   # bottom, left, top, right

  oma = c(0, 0, 0, 0)

)

p_pub_ace1 <- iplot(
  models$pub_ace1,
  main = "ACE I",
  xlab = "Years relative to treatment",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

p_pub_ace2 <- iplot(
  models$pub_ace2,
  main = "ACE II",
  xlab = "Years relative to treatment",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

dev.off()

pub_plot <- p_pub_ace1 + p_pub_ace2 +
  plot_annotation(title = "Publications")

jpeg(
  "pub_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  res = 300
)

par(
  mfrow = c(1, 2),
  mar = c(4, 4, 2, 1)
)

iplot(models$pub_ace1)
iplot(models$pub_ace2)

dev.off()




#=========================================================
# Citations
#=========================================================

p_cit_ace1 <- iplot(
  models$cit_ace1,
  main = "ACE I",
  xlab = "Years relative to treatment",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

p_cit_ace2 <- iplot(
  models$cit_ace2,
  main = "ACE II",
  xlab = "Years relative to treatment",
  ylab = "Log incidence-rate ratio",
  ci_level = 0.95,
  ref.line = 0
)

cit_plot <- p_cit_ace1 + p_cit_ace2 +
  plot_annotation(title = "Citations")

jpeg(
  "cit_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  res = 300
)

par(
  mfrow = c(1, 2),
  mar = c(4, 4, 2, 1)
)

iplot(models$cit_ace1)
iplot(models$cit_ace2)

dev.off()


#=========================================================
# International collaboration
#=========================================================

p_int_ace1 <- iplot(
  models$intl_ace1,
  main = "ACE I",
  xlab = "Years relative to treatment",
  ylab = "Effect",
  ci_level = 0.95,
  ref.line = 0
)

p_int_ace2 <- iplot(
  models$intl_ace2,
  main = "ACE II",
  xlab = "Years relative to treatment",
  ylab = "Effect",
  ci_level = 0.95,
  ref.line = 0
)

int_plot <- p_int_ace1 + p_int_ace2 +
  plot_annotation(title = "International collaboration")


jpeg(
  "int_ace1_ace2_did.jpeg",
  width = 4800,
  height = 2400,
  res = 300
)

par(
  mfrow = c(1, 2),
  mar = c(4, 4, 2, 1)
)

iplot(models$intl_ace1)
iplot(models$intl_ace2)

dev.off()

#=========================================================
# Final figure
#=========================================================

did_plots <-
  pub_plot /
  cit_plot /
  int_plot +
  plot_annotation(
    title = "Propensity-score matched Difference-in-Differences"
  )

did_plots

ggsave(
  "did_iplots_all.jpeg",
  did_plots,
  width = 12,
  height = 14,
  dpi = 300
)



library(cobalt)
library(ggplot2)

# ------------------------------------------------------------
# 1. Standard love plot: before vs after matching
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 2. Save plot
# ------------------------------------------------------------

ggsave(
  "covariate_balance_loveplot.jpeg",
  width = 9,
  height = 6,
  dpi = 300
)
