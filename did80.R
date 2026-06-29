library(dplyr)
library(MatchIt)
library(fixest)
library(cobalt)

# ============================================================
# 1. Build pre-treatment matching dataset (2013)
# ============================================================

match_data <- df_africa %>%
  filter(year == 2013) %>%
  group_by(inst_id) %>%
  summarise(

    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),

    country_code = first(country_code),

    log_pub = log1p(first(n_publication)),
    log_cit = log1p(first(n_citations)),

    age = first(year - founded_date),

    gdp_cap = first(gdp_cap),
    internet = first(internet),
    rd_exp = first(rd_exp),

    intl_share = first(international_share_any),
    inter_share = first(inter_african_share),
    extra_share = first(extra_african_share),

    .groups = "drop"
  ) %>%
  filter(
    complete.cases(.)
  )

# ============================================================
# 2. Propensity score matching
# ============================================================

m.out <- matchit(

  ace1_ever ~

    log_pub +
    log_cit +
    age +
    gdp_cap +
    internet +
    rd_exp +
    intl_share +
    inter_share +
    extra_share,

  data = match_data,

  method = "nearest",

  distance = "logit",

  ratio = 2,

  replace = FALSE
)

summary(m.out)

love.plot(
  m.out,
  threshold = .10
)


library(dplyr)
library(MatchIt)

# 1. Create ever-treated indicator on full panel
df_match_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# Check
table(df_match_base$ace1_ever, useNA = "ifany")

# 2. Build 2013 matching dataset
match_data <- df_match_base %>%
  filter(year == 2013) %>%
  transmute(
    inst_id,
    ace1_ever = as.integer(ace1_ever),
    country_code,

    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    age = pmax(year - founded_date, 0),

    gdp_cap,
    internet,
    rd_exp,

    intl_share  = international_share_any,
    inter_share = inter_african_share,
    extra_share = extra_african_share
  ) %>%
  filter(!is.na(ace1_ever)) %>%
  filter(ace1_ever %in% c(0, 1)) %>%
  tidyr::drop_na()

# Check again
table(match_data$ace1_ever)

# 3. Match
m.out <- matchit(
  ace1_ever ~
    log_pub +
    log_cit +
    age +
    gdp_cap +
    internet +
    rd_exp +
    intl_share +
    inter_share +
    extra_share,
  data = match_data,
  method = "nearest",
  distance = "logit",
  ratio = 2,
  replace = FALSE
)


library(dplyr)
library(tidyr)
library(MatchIt)
library(fixest)
library(cobalt)

# ============================================================
# 1. Create ever-treated ACE I indicator
# ============================================================

df_match_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ============================================================
# 2. Build pre-treatment matching data
#    Use only variables available before treatment
# ============================================================

match_data <- df_match_base %>%
  filter(year == 2013) %>%
  transmute(
    inst_id,
    ace1_ever,
    country_code,
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    age = pmax(year - founded_date, 0),
    gdp_cap,
    internet
  ) %>%
  drop_na()

table(match_data$ace1_ever)

# ============================================================
# 3. Propensity score matching
# ============================================================

m.out <- MatchIt::matchit(
  ace1_ever ~ log_pub + log_cit + age + gdp_cap + internet,
  data = match_data,
  method = "nearest",
  distance = "logit",
  ratio = 2,
  replace = FALSE
)

summary(m.out)

# Optional balance plot
love.plot(m.out, threshold = 0.1)

# ============================================================
# 4. Extract matched universities
# ============================================================

matched_ids <- MatchIt::match.data(m.out)$inst_id

df_matched <- df_match_base %>%
  filter(inst_id %in% matched_ids)

# ============================================================
# 5. Matched DID: dynamic treatment effects
# ============================================================

did_matched <- feols(
  log1p(n_publication) ~
    i(year, ace_1, ref = 2013) |
    inst_id + year,
  data = df_matched,
  cluster = ~ country_code
)

summary(did_matched)

iplot(
  did_matched,
  main = "ACE I DID on matched sample",
  xlab = "Year",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 6. Pre-trend test on matched sample
# ============================================================

pretrend_matched <- feols(
  log1p(n_publication) ~
    i(year, ace1_ever, ref = 2013) |
    inst_id + year,
  data = df_matched,
  cluster = ~ country_code
)

summary(pretrend_matched)

iplot(
  pretrend_matched,
  main = "ACE I pre-trend test on matched sample",
  xlab = "Year",
  ylab = "Effect relative to 2013",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 7. Joint Wald test for pre-treatment coefficients
# ============================================================

pre_coef_names <- names(coef(pretrend_matched))[
  grepl("year::20(06|07|08|09|10|11|12):ace1_ever", names(coef(pretrend_matched)))
]

pretrend_wald <- wald(
  pretrend_matched,
  paste0(pre_coef_names, " = 0")
)

pretrend_wald























# ============================================================
# 3. Extract matched universities
# ============================================================

matched_ids <- match.data(m.out)$inst_id

df_matched <- df_africa %>%
  filter(inst_id %in% matched_ids)

# ============================================================
# 4. Event-study DID on matched sample
# ============================================================

did_match <- feols(

  log1p(n_publication) ~

    i(year, ace_1, ref = 2013) |

    inst_id +
    year,

  cluster = ~country_code,

  data = df_matched

)

summary(did_match)

iplot(
  did_match,
  main = "ACE I Event-study (Matched sample)",
  ci_level = .95,
  ref.line = 0
)

# ============================================================
# 5. Pre-trend test on matched sample
# ============================================================

df_matched <- df_matched %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1))
  ) %>%
  ungroup()

pretrend_match <- feols(

  log1p(n_publication) ~

    i(year, ace1_ever, ref = 2013) |

    inst_id +
    year,

  cluster = ~country_code,

  data = df_matched

)

wald(
  pretrend_match,
  c(
    "year::2006:ace1_ever = 0",
    "year::2007:ace1_ever = 0",
    "year::2008:ace1_ever = 0",
    "year::2009:ace1_ever = 0",
    "year::2010:ace1_ever = 0",
    "year::2011:ace1_ever = 0",
    "year::2012:ace1_ever = 0"
  )
)













library(dplyr)
library(tidyr)
library(fixest)
library(MatchIt)
library(cobalt)
library(ggplot2)

# ============================================================
# 0. ACE I treatment timing
# ============================================================

treat_year <- 2014
ref_year   <- 2013

# ============================================================
# 1. Create ever-treated indicator
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ============================================================
# 2. Construct pre-treatment trajectories, 2006–2013
# ============================================================

pre_trends <- df_base %>%
  filter(year <= ref_year) %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations)
  ) %>%
  group_by(inst_id) %>%
  summarise(
    pub_2013 = log_pub[year == 2013][1],
    cit_2013 = log_cit[year == 2013][1],

    pub_mean_pre = mean(log_pub, na.rm = TRUE),
    cit_mean_pre = mean(log_cit, na.rm = TRUE),

    pub_slope_pre = ifelse(
      sum(!is.na(log_pub)) >= 3,
      coef(lm(log_pub ~ year))[2],
      NA_real_
    ),

    cit_slope_pre = ifelse(
      sum(!is.na(log_cit)) >= 3,
      coef(lm(log_cit ~ year))[2],
      NA_real_
    ),

    .groups = "drop"
  )

# ============================================================
# 3. Build matching dataset
# ============================================================

match_data <- df_base %>%
  filter(year == ref_year) %>%
  select(
    inst_id,
    ace1_ever,
    country_code,
    founded_date,
    gdp_cap,
    internet
  ) %>%
  left_join(pre_trends, by = "inst_id") %>%
  mutate(
    age_2013 = pmax(ref_year - founded_date, 0),
    log_age_2013 = log1p(age_2013)
  ) %>%
  select(
    inst_id,
    ace1_ever,
    country_code,
    pub_2013,
    cit_2013,
    pub_mean_pre,
    cit_mean_pre,
    pub_slope_pre,
    cit_slope_pre,
    log_age_2013,
    gdp_cap,
    internet
  ) %>%
  drop_na()

table(match_data$ace1_ever)

# ============================================================
# 4A. Mahalanobis matching on pre-treatment levels + trends
# ============================================================

m_maha <- MatchIt::matchit(
  ace1_ever ~
    pub_2013 +
    cit_2013 +
    pub_mean_pre +
    cit_mean_pre +
    pub_slope_pre +
    cit_slope_pre +
    log_age_2013 +
    gdp_cap +
    internet,
  data = match_data,
  method = "nearest",
  distance = "mahalanobis",
  ratio = 2,
  replace = FALSE
)

summary(m_maha)
love.plot(m_maha, threshold = 0.1)

matched_ids_maha <- MatchIt::match.data(m_maha)$inst_id

df_maha <- df_base %>%
  filter(inst_id %in% matched_ids_maha)

# ============================================================
# 4B. Coarsened Exact Matching (CEM)
# ============================================================

m_cem <- MatchIt::matchit(
  ace1_ever ~
    pub_2013 +
    cit_2013 +
    pub_slope_pre +
    cit_slope_pre +
    log_age_2013 +
    gdp_cap +
    internet,
  data = match_data,
  method = "cem",
  cutpoints = list(
    pub_2013 = quantile(match_data$pub_2013, probs = c(.25, .50, .75), na.rm = TRUE),
    cit_2013 = quantile(match_data$cit_2013, probs = c(.25, .50, .75), na.rm = TRUE),
    pub_slope_pre = quantile(match_data$pub_slope_pre, probs = c(.25, .50, .75), na.rm = TRUE),
    cit_slope_pre = quantile(match_data$cit_slope_pre, probs = c(.25, .50, .75), na.rm = TRUE),
    log_age_2013 = quantile(match_data$log_age_2013, probs = c(.25, .50, .75), na.rm = TRUE),
    gdp_cap = quantile(match_data$gdp_cap, probs = c(.25, .50, .75), na.rm = TRUE),
    internet = quantile(match_data$internet, probs = c(.25, .50, .75), na.rm = TRUE)
  )
)

summary(m_cem)
love.plot(m_cem, threshold = 0.1)

matched_ids_cem <- MatchIt::match.data(m_cem)$inst_id

df_cem <- df_base %>%
  filter(inst_id %in% matched_ids_cem)

# ============================================================
# 5. Function: DID + pre-trend test on matched sample
# ============================================================

run_matched_did <- function(data, label) {

  did <- feols(
    log1p(n_publication) ~
      i(year, ace_1, ref = 2013) |
      inst_id + year,
    data = data,
    cluster = ~ country_code
  )

  pretrend <- feols(
    log1p(n_publication) ~
      i(year, ace1_ever, ref = 2013) |
      inst_id + year,
    data = data,
    cluster = ~ country_code
  )

  pre_coef_names <- names(coef(pretrend))[
    grepl("year::20(06|07|08|09|10|11|12):ace1_ever", names(coef(pretrend)))
  ]

  pretrend_wald <- wald(
    pretrend,
    paste0(pre_coef_names, " = 0")
  )

  print(paste("==========", label, "=========="))
  print(summary(did))
  print(pretrend_wald)

  iplot(
    did,
    main = paste0("ACE I DID - ", label),
    xlab = "Year",
    ylab = "Effect on log(publications + 1)",
    ref.line = 0,
    ci_level = 0.95
  )

  iplot(
    pretrend,
    main = paste0("ACE I pre-trend test - ", label),
    xlab = "Year",
    ylab = "Effect relative to 2013",
    ref.line = 0,
    ci_level = 0.95
  )

  list(
    did = did,
    pretrend = pretrend,
    pretrend_wald = pretrend_wald
  )
}

# ============================================================
# 6. Run DID after matching
# ============================================================

results_maha <- run_matched_did(df_maha, "Mahalanobis matched sample")
results_cem  <- run_matched_did(df_cem, "CEM matched sample")

# ============================================================
# 7. Regression table
# ============================================================

etable(
  results_maha$did,
  results_cem$did,
  dict = c(
    "year::2014:ace_1" = "ACE I × 2014",
    "year::2015:ace_1" = "ACE I × 2015",
    "year::2016:ace_1" = "ACE I × 2016",
    "year::2017:ace_1" = "ACE I × 2017",
    "year::2018:ace_1" = "ACE I × 2018",
    "year::2019:ace_1" = "ACE I × 2019",
    "year::2020:ace_1" = "ACE I × 2020",
    "year::2021:ace_1" = "ACE I × 2021",
    "year::2022:ace_1" = "ACE I × 2022",
    "year::2023:ace_1" = "ACE I × 2023",
    "year::2024:ace_1" = "ACE I × 2024",
    "year::2025:ace_1" = "ACE I × 2025"
  ),
  fitstat = ~ n + r2 + ar2,
  se.below = TRUE,
  tex = TRUE,
  file = "ace1_matched_did_table.tex"
)



summary(m_cem)


df_africa |>
  group_by(inst_id) |>
  summarise(ace1 = any(ace_1 == 1)) |>
  summarise(n_treated = sum(ace1))





library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)

# ============================================================
# 1. Ever-treated indicator
# ============================================================

df_base <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ============================================================
# 2. Estimate pre-treatment publication trend
# ============================================================

pub_trend <- df_base %>%
  filter(year <= 2013) %>%
  mutate(
    log_pub = log1p(n_publication)
  ) %>%
  group_by(inst_id) %>%
  summarise(

    pub_2013 = log_pub[year == 2013][1],

    pub_slope = if(sum(!is.na(log_pub)) >= 3)
      coef(lm(log_pub ~ year))[2]
    else
      NA_real_,

    .groups = "drop"
  )

# ============================================================
# 3. Matching data
# ============================================================

match_data <- df_base %>%
  filter(year == 2013) %>%
  select(
    inst_id,
    ace1_ever,
    country_code,
    founded_date,
    gdp_cap,
    internet
  ) %>%
  left_join(pub_trend, by = "inst_id") %>%
  mutate(

    log_age = log1p(pmax(2013 - founded_date, 0))

  ) %>%
  select(
    inst_id,
    ace1_ever,
    country_code,
    pub_2013,
    pub_slope,
    log_age,
    gdp_cap,
    internet
  ) %>%
  drop_na()

table(match_data$ace1_ever)

# ============================================================
# 4. Mahalanobis matching
# ============================================================

m.out <- matchit(

  ace1_ever ~

    pub_2013 +
    pub_slope +
    log_age +
    gdp_cap +
    internet,

  data = match_data,

  method = "nearest",

  distance = "mahalanobis",

  ratio = 3,

  replace = FALSE

)

summary(m.out)

love.plot(
  m.out,
  threshold = .10
)

# ============================================================
# 5. Matched sample
# ============================================================

matched_ids <- match.data(m.out)$inst_id

df_matched <- df_base %>%
  filter(inst_id %in% matched_ids)

# ============================================================
# 6. Regression controls
# ============================================================

df_matched <- df_matched %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(

    L2_log_citations = lag(log1p(n_citations), 2),

    L2_inter =
      lag(as.integer(n_inter_african > 0), 2),

    L2_extra =
      lag(as.integer(n_extra_african > 0), 2)

  ) %>%
  ungroup() %>%
  mutate(

    country_year =
      interaction(country_code, year, drop = TRUE)

  )

# ============================================================
# 7. Main DID
# ============================================================

did_match <- feols(

  log1p(n_publication) ~

    i(year, ace_1, ref = 2013) +

    L2_log_citations +
    L2_inter +
    L2_extra |

    inst_id +
    country_year,

  cluster = ~country_code,

  data = df_matched

)

summary(did_match)

iplot(
  did_match,
  main = "ACE I DID after structural matching",
  ci_level = .95,
  ref.line = 0
)

# ============================================================
# 8. Pre-trend test
# ============================================================

pretrend <- feols(

  log1p(n_publication) ~

    i(year, ace1_ever, ref = 2013) +

    L2_log_citations +
    L2_inter +
    L2_extra |

    inst_id +
    country_year,

  cluster = ~country_code,

  data = df_matched

)

summary(pretrend)

iplot(
  pretrend,
  main = "ACE I pre-trends after matching",
  ci_level = .95,
  ref.line = 0
)

# ============================================================
# 9. Joint test of pre-treatment coefficients
# ============================================================

wald(
  pretrend,
  keep = "year::20(06|07|08|09|10|11|12)"
)


df_ace1_vs_ace2 <- df_africa %>%
  filter(ace_1 == 1 | ace_2 == 1 | year < 2016) %>%
  mutate(
    ace1_group = as.integer(ace_1 == 1),
    post_ace1  = as.integer(year >= 2014),
    did_ace1   = ace1_group * post_ace1
  ) %>%
  filter(year <= 2015)

m_ace1_vs_ace2 <- feols(
  log1p(n_publication) ~ did_ace1 |
    inst_id + year,
  data = df_ace1_vs_ace2,
  cluster = ~ country_code
)

summary(m_ace1_vs_ace2)


pretrend_ace1_ace2 <- feols(
  log1p(n_publication) ~ i(year, ace1_group, ref = 2013) |
    inst_id + year,
  data = df_ace1_vs_ace2 %>% filter(year <= 2013),
  cluster = ~ country_code
)

iplot(pretrend_ace1_ace2)











library(dplyr)
library(tidyr)
library(fixest)
library(MatchIt)
library(cobalt)
library(ggplot2)

# ============================================================
# 1. Build treatment timing
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
# 2. Pre-treatment characteristics for matching
# ============================================================

pre_chars <- df_base %>%
  filter(year <= 2013) %>%
  mutate(log_pub = log1p(n_publication)) %>%
  group_by(inst_id) %>%
  summarise(
    pub_2013 = log_pub[year == 2013][1],
    pub_slope = if (sum(!is.na(log_pub)) >= 3) {
      coef(lm(log_pub ~ year))[2]
    } else {
      NA_real_
    },
    .groups = "drop"
  )

match_data <- df_base %>%
  filter(year == 2013) %>%
  select(inst_id, treated_ever, country_code, founded_date, gdp_cap, internet) %>%
  left_join(pre_chars, by = "inst_id") %>%
  mutate(
    log_age = log1p(pmax(2013 - founded_date, 0))
  ) %>%
  select(
    inst_id, treated_ever, country_code,
    pub_2013, pub_slope, log_age, gdp_cap, internet
  ) %>%
  drop_na()

table(match_data$treated_ever)

# ============================================================
# 3. Match treated universities to never-treated controls
# ============================================================

m.out <- matchit(
  treated_ever ~ pub_2013 + pub_slope + log_age + gdp_cap + internet,
  data = match_data,
  method = "nearest",
  distance = "mahalanobis",
  ratio = 3,
  replace = FALSE
)

summary(m.out)
love.plot(m.out, threshold = 0.1)

matched_controls <- match.data(m.out) %>%
  filter(treated_ever == 0) %>%
  pull(inst_id)

treated_ids <- df_base %>%
  filter(treated_ever == 1) %>%
  distinct(inst_id) %>%
  pull(inst_id)

matched_ids <- c(treated_ids, matched_controls)

df_matched_base <- df_base %>%
  filter(inst_id %in% matched_ids)

# ============================================================
# 4. Build stacked DID data
#    Stack 1: ACE I cohort, treatment year 2014
#    Stack 2: ACE II cohort, treatment year 2016
# ============================================================

stack_ace1 <- df_matched_base %>%
  filter(
    ace1_ever == 1 |
      treated_ever == 0 |
      (ace2_ever == 1 & year < 2016)
  ) %>%
  mutate(
    stack = "ACE I",
    cohort_year = 2014,
    stack_treated = ace1_ever,
    post = as.integer(year >= 2014),
    did = stack_treated * post,
    event_time = year - 2014
  )

stack_ace2 <- df_matched_base %>%
  filter(
    ace2_ever == 1 |
      treated_ever == 0
  ) %>%
  mutate(
    stack = "ACE II",
    cohort_year = 2016,
    stack_treated = ace2_ever,
    post = as.integer(year >= 2016),
    did = stack_treated * post,
    event_time = year - 2016
  )

df_stack <- bind_rows(stack_ace1, stack_ace2) %>%
  mutate(
    stack_inst = interaction(stack, inst_id, drop = TRUE),
    stack_year = interaction(stack, year, drop = TRUE),
    country_year = interaction(country_code, year, drop = TRUE)
  )

# ============================================================
# 5. Stacked DID
# ============================================================

stacked_did <- feols(
  log1p(n_publication) ~ did |
    stack_inst + stack_year,
  data = df_stack,
  cluster = ~ country_code
)

summary(stacked_did)

# ============================================================
# 6. Stacked event-study
# ============================================================

stacked_event <- feols(
  log1p(n_publication) ~
    i(event_time, stack_treated, ref = -1) |
    stack_inst + stack_year,
  data = df_stack,
  cluster = ~ country_code
)

summary(stacked_event)

iplot(
  stacked_event,
  main = "Stacked DID event-study: ACE I and ACE II",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 7. Pre-trend joint test
# ============================================================

pre_coef_names <- names(coef(stacked_event))[
  grepl("event_time::-", names(coef(stacked_event))) &
    !grepl("event_time::-1", names(coef(stacked_event)))
]

pretrend_test <- wald(
  stacked_event,
  paste0(pre_coef_names, " = 0")
)

pretrend_test




library(dplyr)
library(fixest)

# ============================================================
# 1. Build treatment cohort variable
# ============================================================

df_sa <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    
    # Cohort / first treatment year
    treat_year = case_when(
      ace1_ever == 1 ~ 2014,
      ace2_ever == 1 ~ 2016,
      TRUE ~ NA_real_   # never-treated
    )
  ) %>%
  ungroup()

# Check cohorts
table(df_sa$treat_year, useNA = "ifany")

# ============================================================
# 2. Sun & Abraham event-study: publications
# ============================================================

sa_pub <- feols(
  log1p(n_publication) ~
    sunab(treat_year, year) |
    inst_id + year,
  data = df_sa,
  cluster = ~ country_code
)

summary(sa_pub)

iplot(
  sa_pub,
  main = "Sun & Abraham event-study: ACE programs",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 3. With controls
# ============================================================

df_sa <- df_sa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    L2_log_citations = lag(log1p(n_citations), 2),
    L2_inter = lag(as.integer(n_inter_african > 0), 2),
    L2_extra = lag(as.integer(n_extra_african > 0), 2)
  ) %>%
  ungroup()

sa_pub_controls <- feols(
  log1p(n_publication) ~
    sunab(treat_year, year) +
    L2_log_citations +
    L2_inter +
    L2_extra |
    inst_id + year,
  data = df_sa,
  cluster = ~ country_code
)

summary(sa_pub_controls)

iplot(
  sa_pub_controls,
  main = "Sun & Abraham event-study with controls",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 4. Country-year FE version
# ============================================================

df_sa <- df_sa %>%
  mutate(
    country_year = interaction(country_code, year, drop = TRUE)
  )

sa_pub_cy <- feols(
  log1p(n_publication) ~
    sunab(treat_year, year) +
    L2_log_citations +
    L2_inter +
    L2_extra |
    inst_id + country_year,
  data = df_sa,
  cluster = ~ country_code
)

summary(sa_pub_cy)

iplot(
  sa_pub_cy,
  main = "Sun & Abraham event-study with country-year FE",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 5. Pre-trend joint test
# ============================================================

pre_coef_names <- names(coef(sa_pub_cy))[
  grepl("year::-", names(coef(sa_pub_cy))) &
    !grepl("year::-1", names(coef(sa_pub_cy)))
]

wald(
  sa_pub_cy,
  paste0(pre_coef_names, " = 0")
)

# ============================================================
# 6. Average treatment effect
# ============================================================

etable(
  sa_pub,
  sa_pub_controls,
  sa_pub_cy,
  dict = c(
    L2_log_citations = "Log citations (t-2)",
    L2_inter = "Inter-African collaboration (t-2)",
    L2_extra = "Extra-African collaboration (t-2)"
  ),
  fitstat = ~ n + r2 + ar2,
  se.below = TRUE
)



library(dplyr)

selection_table <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1))
  ) %>%
  ungroup() %>%
  filter(year == 2013) %>%
  summarise(
    Publications = mean(log1p(n_publication[ace1_ever == 1])) -
                   mean(log1p(n_publication[ace1_ever == 0])),
    Citations = mean(log1p(n_citations[ace1_ever == 1])) -
                mean(log1p(n_citations[ace1_ever == 0]))
  )



df_plot <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = any(ace_1==1)
  ) %>%
  group_by(year, ace1_ever) %>%
  summarise(
    mean_pub = mean(log1p(n_publication), na.rm=TRUE)
  )

ggplot(df_plot,
       aes(year, mean_pub,
           colour=factor(ace1_ever)))+
geom_line(size=1.3)



df_plot <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = any(ace_1 == 1)
  ) %>%
  ungroup() %>%
  group_by(year, ace1_ever) %>%
  summarise(
    mean_pub = mean(log1p(n_publication), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ace1_ever) %>%
  mutate(
    mean_pub = mean_pub - first(mean_pub)
  )



library(dplyr)
library(ggplot2)

df_plot <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = any(ace_1 == 1)
  ) %>%
  ungroup() %>%
  group_by(year, ace1_ever) %>%
  summarise(
    mean_pub = mean(log1p(n_publication), na.rm = TRUE),
    se = sd(log1p(n_publication), na.rm = TRUE) /
      sqrt(sum(!is.na(n_publication))),
    .groups = "drop"
  ) %>%
  group_by(ace1_ever) %>%
  mutate(
    mean_pub = mean_pub - first(mean_pub),
    ymin = mean_pub - se,
    ymax = mean_pub + se
  ) %>%
  ungroup()

ggplot(
  df_plot,
  aes(
    x = year,
    y = mean_pub,
    colour = factor(ace1_ever),
    fill = factor(ace1_ever)
  )
) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1.3) +
  geom_vline(
    xintercept = 2014,
    linetype = "dashed",
    colour = "black"
  ) +
  scale_colour_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
  ) +
  scale_fill_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
  ) +
  labs(
    x = "Year",
    y = "Change in log(publications + 1)",
    colour = "",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none"
  )




library(dplyr)
library(tidyr)
library(ggplot2)

df_plot <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = any(ace_1 == 1, na.rm = TRUE)
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
    ace1_ever,
    publications,
    citations,
    international_collaboration
  ) %>%
  pivot_longer(
    cols = c(publications, citations, international_collaboration),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(year, ace1_ever, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  ) %>%
  group_by(ace1_ever, metric) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    baseline = first(mean_value),
    mean_value = mean_value - baseline,
    ymin = mean_value - se,
    ymax = mean_value + se
  ) %>%
  ungroup() %>%
  mutate(
    metric = recode(
      metric,
      publications = "Publications",
      citations = "Citations",
      international_collaboration = "International collaboration"
    )
  )

p_three_metrics <- ggplot(
  df_plot,
  aes(
    x = year,
    y = mean_value,
    colour = factor(ace1_ever),
    fill = factor(ace1_ever)
  )
) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = 2014,
    linetype = "dashed",
    colour = "black"
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    nrow = 1
  ) +
  scale_colour_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
  ) +
  scale_fill_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
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

p_three_metrics



library(dplyr)
library(tidyr)
library(ggplot2)

df_plot_metrics <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace2_ever = any(ace_2 == 1, na.rm = TRUE),
    ace1_ever = any(ace_1 == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    publications = log1p(n_publication),
    citations = log1p(n_citations),
    international_collaboration = international_share_any
  ) %>%
  dplyr::select(
    inst_id,
    year,
    ace1_ever,
    publications,
    citations,
    international_collaboration
  ) %>%
  pivot_longer(
    cols = c(publications, citations, international_collaboration),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(year, ace1_ever, ace2_ever, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  ) %>%
  group_by(ace1_ever, ace2_ever, metric) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    baseline = mean_value[year == min(year)],
    mean_value = mean_value - baseline,
    ymin = mean_value - se,
    ymax = mean_value + se
  ) %>%
  ungroup() %>%
  mutate(
    metric = dplyr::recode(
      metric,
      publications = "Publications",
      citations = "Citations",
      international_collaboration = "International collaboration"
    )
  )

p_metrics <- ggplot(
  data = df_plot_metrics,
  aes(
    x = year,
    y = mean_value,
    colour = factor(ace1_ever),
    fill = factor(ace1_ever)
  )
) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1.3) +
  geom_vline(
    data = distinct(df_plot_metrics, metric),
    aes(xintercept = 2014),
    inherit.aes = FALSE,
    linetype = "dashed",
    colour = "black"
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    nrow = 1
  ) +
  scale_colour_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
  ) +
  scale_fill_manual(
    values = c("#D55E00", "#0072B2"),
    labels = c("Never treated", "ACE I")
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

p_metrics


library(dplyr)
library(tidyr)
library(ggplot2)

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

p_metrics

ggsave('p_metrics.jpeg', p_metrics, width = 16, height = 9, dpi = 500)
