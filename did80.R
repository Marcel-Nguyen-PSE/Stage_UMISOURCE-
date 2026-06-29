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


df_africa <- read_csv('df_africa.csv')

library(dplyr)

# Universities ever treated by ACE I
ace2_universities <- df_africa %>%
  group_by(inst_id) %>%
  summarise(
    ace2_ever = any(ace_2 == 1, na.rm = TRUE),
    name = first(name),
    country = first(country_code),
    .groups = "drop"
  ) %>%
  filter(ace2_ever)

# Number of treated universities
nrow(ace2_universities)

# Display them
ace1_universities %>%
  arrange(country, name)











library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(purrr)
library(httr2)
library(tibble)

# ------------------------------------------------------------
# 1. ACE II official host universities
# ------------------------------------------------------------

ace2_official <- tribble(
  ~official_name, ~country_iso3,
  "Haramaya University", "ETH",
  "Addis Ababa University", "ETH",
  "University of Malawi", "MWI",
  "Egerton University", "KEN",
  "Jaramogi Oginga Odinga University of Science and Technology", "KEN",
  "Moi University", "KEN",
  "University of Rwanda", "RWA",
  "Sokoine University of Agriculture", "TZA",
  "Nelson Mandela African Institution of Science and Technology", "TZA",
  "Kamuzu University", "MWI",
  "Universidade Eduardo Mondlane", "MOZ",
  "Mzuzu University", "MWI",
  "Makerere University", "UGA",
  "Uganda Martyrs University", "UGA",
  "Mbarara University of Science and Technology", "UGA",
  "University of Zambia", "ZMB",
  "Copperbelt University", "ZMB",
  "Lilongwe University of Agriculture and Natural Resources", "MWI"
)

# ------------------------------------------------------------
# 2. Normalize names
# ------------------------------------------------------------

norm_name <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

df_names <- df_africa %>%
  distinct(inst_id, name, country_code) %>%
  mutate(name_norm = norm_name(name))

# ------------------------------------------------------------
# 3. Fuzzy match ACE II names inside df_africa
# ------------------------------------------------------------

ace2_local_match <- ace2_official %>%
  mutate(official_norm = norm_name(official_name)) %>%
  rowwise() %>%
  mutate(
    candidates = list(
      df_names %>%
        filter(country_code == country_iso3) %>%
        mutate(score = adist(official_norm, name_norm)[1, ]) %>%
        arrange(score) %>%
        slice_head(n = 5)
    )
  ) %>%
  ungroup() %>%
  unnest(candidates)

ace2_local_match %>%
  select(official_name, country_iso3, inst_id, name, country_code, score) %>%
  arrange(official_name, score)

# ------------------------------------------------------------
# 4. Keep best local match
# ------------------------------------------------------------

ace2_ids <- ace2_local_match %>%
  group_by(official_name, country_iso3) %>%
  slice_min(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    official_name,
    country_iso3,
    inst_id,
    matched_name = name,
    country_code,
    score
  )

ace2_ids

# ------------------------------------------------------------
# 5. Optional OpenAlex API check
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

search_openalex_inst <- function(query) {
  req <- request("https://api.openalex.org/institutions") %>%
    req_url_query(search = query, `per-page` = 5) %>%
    req_perform()

  resp <- resp_body_json(req)

  tibble(
    query = query,
    openalex_id = map_chr(resp$results, ~ basename(.x$id)),
    display_name = map_chr(resp$results, ~ .x$display_name %||% NA_character_),
    country_code = map_chr(resp$results, ~ .x$country_code %||% NA_character_),
    works_count = map_int(resp$results, ~ .x$works_count %||% NA_integer_)
  )
}

openalex_check_ace2 <- map_dfr(
  ace2_official$official_name,
  search_openalex_inst
)

openalex_check_ace2

# ------------------------------------------------------------
# 6. Final ACE II OpenAlex IDs
#    Inspect ace2_ids before trusting this vector.
# ------------------------------------------------------------

ace2_openalex_ids <- ace2_ids %>%
  pull(inst_id) %>%
  unique()

length(ace2_openalex_ids)
ace2_openalex_ids

# ------------------------------------------------------------
# 7. Correct ACE II dummy
# ------------------------------------------------------------

df_africa <- df_africa %>%
  mutate(
    ace_2_old = ace_2,
    ace_2 = as.integer(inst_id %in% ace2_openalex_ids & year >= 2016),
    ace2_ever = as.integer(inst_id %in% ace2_openalex_ids)
  )

# ------------------------------------------------------------
# 8. Check result
# ------------------------------------------------------------

df_africa %>%
  filter(ace2_ever == 1) %>%
  distinct(inst_id, name, country_code) %>%
  arrange(country_code, name)

df_africa %>%
  filter(ace2_ever == 1) %>%
  distinct(inst_id) %>%
  nrow()


library(dplyr)
library(stringr)
library(purrr)
library(httr2)
library(tibble)

# ------------------------------------------------------------
# 1. Official ACE I host universities
# ------------------------------------------------------------

ace1_official <- tribble(
  ~official_name, ~country_iso3,
  "Redeemer's University", "NGA",
  "Institut National Polytechnique Félix Houphouët-Boigny", "CIV",
  "University of Port Harcourt", "NGA",
  "École Nationale Supérieure de Statistique et d'Économie Appliquée", "CIV",
  "Kwame Nkrumah University of Science and Technology", "GHA",
  "Ahmadu Bello University", "NGA",
  "Université de Yaoundé I", "CMR",
  "International Institute for Water and Environmental Engineering", "BFA",
  "Federal University of Agriculture, Abeokuta", "NGA",
  "Université Cheikh Anta Diop", "SEN",
  "Université de Lomé", "TGO",
  "Obafemi Awolowo University", "NGA",
  "University of Ghana", "GHA",
  "University of Jos", "NGA",
  "Université d'Abomey-Calavi", "BEN",
  "University of Benin", "NGA",
  "African University of Science and Technology", "NGA",
  "Université Félix Houphouët-Boigny", "CIV",
  "Benue State University", "NGA",
  "Bayero University", "NGA",
  "Université Gaston Berger", "SEN"
)

# Note: official ACE I has 22 centres, but University of Ghana hosts two centres.
# Therefore the host-university list has 21 distinct host universities.

# ------------------------------------------------------------
# 2. Search OpenAlex IDs from your own df_africa first
# ------------------------------------------------------------

norm_name <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

df_names <- df_africa %>%
  distinct(inst_id, name, country_code) %>%
  mutate(name_norm = norm_name(name))

ace1_local_match <- ace1_official %>%
  mutate(official_norm = norm_name(official_name)) %>%
  rowwise() %>%
  mutate(
    candidates = list(
      df_names %>%
        filter(country_code == country_iso3) %>%
        mutate(score = adist(official_norm, name_norm)[1, ]) %>%
        arrange(score) %>%
        slice_head(n = 5)
    )
  ) %>%
  ungroup() %>%
  tidyr::unnest(candidates)

ace1_local_match %>%
  select(official_name, country_iso3, inst_id, name, country_code, score) %>%
  arrange(official_name, score)

# ------------------------------------------------------------
# 3. Manually validate the best match for each institution
# ------------------------------------------------------------

ace1_ids <- ace1_local_match %>%
  group_by(official_name, country_iso3) %>%
  slice_min(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(official_name, country_iso3, inst_id, matched_name = name, score)

ace1_ids

# ------------------------------------------------------------
# 4. Optional: search OpenAlex API for missing / suspicious matches
# ------------------------------------------------------------

search_openalex_inst <- function(query) {
  req <- request("https://api.openalex.org/institutions") %>%
    req_url_query(search = query, per-page = 5) %>%
    req_perform()

  resp <- resp_body_json(req)

  tibble(
    query = query,
    openalex_id = map_chr(resp$results, ~ basename(.x$id)),
    display_name = map_chr(resp$results, ~ .x$display_name %||% NA_character_),
    country_code = map_chr(resp$results, ~ .x$country_code %||% NA_character_),
    works_count = map_int(resp$results, ~ .x$works_count %||% NA_integer_)
  )
}

openalex_check <- map_dfr(
  ace1_official$official_name,
  search_openalex_inst
)

openalex_check

# ------------------------------------------------------------
# 5. Final ACE I ID vector
#    Use this after manually checking ace1_ids / openalex_check.
# ------------------------------------------------------------

ace1_openalex_ids <- ace1_ids %>%
  pull(inst_id) %>%
  unique()

length(ace1_openalex_ids)
ace1_openalex_ids

# ------------------------------------------------------------
# 6. Correct ACE I dummy in df_africa
# ------------------------------------------------------------

df_africa <- df_africa %>%
  mutate(
    ace_1_old = ace_1,
    ace_1 = as.integer(inst_id %in% ace1_openalex_ids & year >= 2014),
    ace1_ever = as.integer(inst_id %in% ace1_openalex_ids)
  )

# ------------------------------------------------------------
# 7. Check result
# ------------------------------------------------------------

df_africa %>%
  filter(ace1_ever == 1) %>%
  distinct(inst_id, name, country_code) %>%
  arrange(country_code, name)

df_africa %>%
  filter(ace1_ever == 1) %>%
  distinct(inst_id) %>%
  nrow()




# ============================================================
# Synthetic DID for ACE programs
# Outcome: n_publication
# Package: synthdid
# ============================================================

library(dplyr)
library(tidyr)
library(synthdid)
library(ggplot2)

# install.packages("synthdid") if needed

# ------------------------------------------------------------
# 1. Prepare treatment timing
# ------------------------------------------------------------

df_sdid <- df_africa %>%
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
  ungroup() %>%
  mutate(
    y = log1p(n_publication)
  ) %>%
  filter(!is.na(y), !is.na(inst_id), !is.na(year))

# ------------------------------------------------------------
# 2. Function: build balanced panel for one cohort
# ------------------------------------------------------------

make_sdid_panel <- function(data, cohort_year, min_year, max_year) {
  
  df_cohort <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    mutate(
      treated = as.integer(treat_year == cohort_year),
      control = as.integer(is.na(treat_year))
    ) %>%
    filter(treated == 1 | control == 1) %>%
    select(inst_id, year, y, treated)
  
  # Keep only balanced units
  balanced_ids <- df_cohort %>%
    group_by(inst_id) %>%
    summarise(
      n_years = n_distinct(year),
      .groups = "drop"
    ) %>%
    filter(n_years == length(min_year:max_year)) %>%
    pull(inst_id)
  
  df_cohort <- df_cohort %>%
    filter(inst_id %in% balanced_ids)
  
  # Wide outcome matrix: rows = units, columns = years
  Y <- df_cohort %>%
    select(inst_id, year, y) %>%
    pivot_wider(
      names_from = year,
      values_from = y
    ) %>%
    arrange(inst_id)
  
  unit_ids <- Y$inst_id
  
  Y_mat <- Y %>%
    select(-inst_id) %>%
    as.matrix()
  
  # Treatment indicator by unit
  treated_vec <- df_cohort %>%
    distinct(inst_id, treated) %>%
    arrange(inst_id) %>%
    pull(treated)
  
  # synthdid requires controls first, treated second
  order_units <- order(treated_vec)
  
  Y_mat <- Y_mat[order_units, ]
  treated_vec <- treated_vec[order_units]
  unit_ids <- unit_ids[order_units]
  
  N0 <- sum(treated_vec == 0)
  T0 <- sum(as.integer(colnames(Y_mat)) < cohort_year)
  
  list(
    Y = Y_mat,
    N0 = N0,
    T0 = T0,
    unit_ids = unit_ids,
    treated_vec = treated_vec,
    cohort_year = cohort_year
  )
}

# ------------------------------------------------------------
# 3. ACE I synthetic DID
#    ACE I treated in 2014, controls = never-treated
# ------------------------------------------------------------

panel_ace1 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2014,
  min_year = 2006,
  max_year = 2025
)

sdid_ace1 <- synthdid_estimate(
  Y = panel_ace1$Y,
  N0 = panel_ace1$N0,
  T0 = panel_ace1$T0
)

summary(sdid_ace1)

plot(
  sdid_ace1,
  overlay = 1,
  main = "Synthetic DID: ACE I effect on publications"
)

# ------------------------------------------------------------
# 4. ACE II synthetic DID
#    ACE II treated in 2016, controls = never-treated
# ------------------------------------------------------------

panel_ace2 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2016,
  min_year = 2006,
  max_year = 2025
)

sdid_ace2 <- synthdid_estimate(
  Y = panel_ace2$Y,
  N0 = panel_ace2$N0,
  T0 = panel_ace2$T0
)

summary(sdid_ace2)

plot(
  sdid_ace2,
  overlay = 1,
  main = "Synthetic DID: ACE II effect on publications"
)

# ------------------------------------------------------------
# 5. Placebo standard errors
# ------------------------------------------------------------

se_ace1 <- sqrt(vcov(sdid_ace1, method = "placebo"))
se_ace2 <- sqrt(vcov(sdid_ace2, method = "placebo"))

se_ace1
se_ace2

# ------------------------------------------------------------
# 6. Compact results table
# ------------------------------------------------------------

sdid_results <- tibble(
  cohort = c("ACE I", "ACE II"),
  treatment_year = c(2014, 2016),
  estimate = c(as.numeric(sdid_ace1), as.numeric(sdid_ace2)),
  se_placebo = c(as.numeric(se_ace1), as.numeric(se_ace2)),
  t_stat = estimate / se_placebo,
  p_value = 2 * pnorm(-abs(t_stat))
)

sdid_results

# ------------------------------------------------------------
# 7. Optional: restrict ACE I controls to never-treated + ACE II before 2016
#    This gives only short-run ACE I effect, 2014–2015.
# ------------------------------------------------------------

df_sdid_ace1_short <- df_sdid %>%
  filter(year <= 2015) %>%
  mutate(
    treated = as.integer(ace1_ever == 1),
    control = as.integer(ace2_ever == 1 | treated_ever == 0)
  ) %>%
  filter(treated == 1 | control == 1)

panel_ace1_short <- make_sdid_panel(
  data = df_sdid_ace1_short %>%
    mutate(
      treat_year = if_else(treated == 1, 2014, NA_real_)
    ),
  cohort_year = 2014,
  min_year = 2006,
  max_year = 2015
)

sdid_ace1_short <- synthdid_estimate(
  Y = panel_ace1_short$Y,
  N0 = panel_ace1_short$N0,
  T0 = panel_ace1_short$T0
)

summary(sdid_ace1_short)

plot(
  sdid_ace1_short,
  overlay = 1,
  main = "Synthetic DID: ACE I short-run effect, controls include ACE II"
)









library(dplyr)
library(tidyr)
library(synthdid)

# ============================================================
# 1. Prepare clean data
# ============================================================

df_sdid <- df_africa %>%
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
  ungroup() %>%
  mutate(y = log1p(n_publication)) %>%
  filter(!is.na(y), !is.na(inst_id), !is.na(year)) %>%
  group_by(inst_id, year) %>%
  summarise(
    y = mean(y, na.rm = TRUE),
    treat_year = first(na.omit(treat_year)),
    country_code = first(country_code),
    .groups = "drop"
  )

# Fix for never-treated units where first(na.omit()) returned nothing
df_sdid <- df_sdid %>%
  group_by(inst_id) %>%
  mutate(
    treat_year = ifelse(all(is.na(treat_year)), NA_real_, first(na.omit(treat_year)))
  ) %>%
  ungroup()

# ============================================================
# 2. Robust SDID panel function
# ============================================================

make_sdid_panel <- function(data, cohort_year, min_year, max_year) {

  years <- min_year:max_year

  df_cohort <- data %>%
    filter(year %in% years) %>%
    mutate(
      treated = as.integer(treat_year == cohort_year),
      control = as.integer(is.na(treat_year))
    ) %>%
    filter(treated == 1 | control == 1)

  # Diagnostic
  print(table(df_cohort$treated, useNA = "ifany"))

  balanced_ids <- df_cohort %>%
    group_by(inst_id) %>%
    summarise(
      n_years = n_distinct(year),
      treated = max(treated, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_years == length(years)) %>%
    pull(inst_id)

  df_cohort <- df_cohort %>%
    filter(inst_id %in% balanced_ids)

  unit_treat <- df_cohort %>%
    distinct(inst_id, treated) %>%
    arrange(treated, inst_id)

  N0 <- sum(unit_treat$treated == 0)
  N1 <- sum(unit_treat$treated == 1)

  cat("Controls:", N0, "\n")
  cat("Treated:", N1, "\n")

  if (N0 == 0) stop("No control units retained.")
  if (N1 == 0) stop("No treated units retained.")

  Y <- df_cohort %>%
    inner_join(unit_treat, by = c("inst_id", "treated")) %>%
    arrange(treated, inst_id, year) %>%
    select(inst_id, year, y) %>%
    pivot_wider(
      names_from = year,
      values_from = y
    ) %>%
    arrange(inst_id)

  # Reorder controls first, treated second
  unit_order <- unit_treat %>%
    mutate(order_id = row_number())

  Y <- unit_treat %>%
    select(inst_id) %>%
    left_join(Y, by = "inst_id")

  Y_mat <- Y %>%
    select(-inst_id) %>%
    as.matrix()

  storage.mode(Y_mat) <- "numeric"

  T0 <- sum(as.integer(colnames(Y_mat)) < cohort_year)

  list(
    Y = Y_mat,
    N0 = N0,
    T0 = T0,
    unit_ids = Y$inst_id
  )
}

# ============================================================
# 3. ACE I SDID
# ============================================================

panel_ace1 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2014,
  min_year = 2006,
  max_year = 2025
)

sdid_ace1 <- synthdid_estimate(
  Y = panel_ace1$Y,
  N0 = panel_ace1$N0,
  T0 = panel_ace1$T0
)

summary(sdid_ace1)

plot(
  sdid_ace1,
  overlay = 1,
  main = "Synthetic DID: ACE I"
)

# ============================================================
# 4. ACE II SDID
# ============================================================

panel_ace2 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2016,
  min_year = 2006,
  max_year = 2025
)

sdid_ace2 <- synthdid_estimate(
  Y = panel_ace2$Y,
  N0 = panel_ace2$N0,
  T0 = panel_ace2$T0
)

summary(sdid_ace2)

plot(
  sdid_ace2,
  overlay = 1,
  main = "Synthetic DID: ACE II"
)





library(dplyr)
library(tidyr)
library(synthdid)

# ============================================================
# 1. Clean panel: one row per inst_id-year
# ============================================================

df_sdid <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE)),
    treat_year_unit = case_when(
      ace1_ever == 1 ~ 2014,
      ace2_ever == 1 ~ 2016,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    y = log1p(n_publication)
  ) %>%
  filter(!is.na(inst_id), !is.na(year), !is.na(y)) %>%
  group_by(inst_id, year) %>%
  summarise(
    y = mean(y, na.rm = TRUE),
    treat_year = first(treat_year_unit),
    country_code = first(country_code),
    .groups = "drop"
  )

# ============================================================
# 2. SDID panel function
# ============================================================

make_sdid_panel <- function(data, cohort_year, min_year, max_year) {

  years <- min_year:max_year

  df_cohort <- data %>%
    filter(year %in% years) %>%
    mutate(
      treated = as.integer(!is.na(treat_year) & treat_year == cohort_year),
      control = as.integer(is.na(treat_year))
    ) %>%
    filter(treated == 1 | control == 1)

  cat("\nRaw unit-year counts:\n")
  print(table(df_cohort$treated, useNA = "ifany"))

  balanced_ids <- df_cohort %>%
    group_by(inst_id) %>%
    summarise(
      n_years = n_distinct(year),
      treated = max(treated, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_years == length(years)) %>%
    pull(inst_id)

  df_cohort <- df_cohort %>%
    filter(inst_id %in% balanced_ids)

  unit_treat <- df_cohort %>%
    group_by(inst_id) %>%
    summarise(
      treated = max(treated, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(treated, inst_id)

  N0 <- sum(unit_treat$treated == 0)
  N1 <- sum(unit_treat$treated == 1)

  cat("\nBalanced units:\n")
  cat("Controls:", N0, "\n")
  cat("Treated:", N1, "\n")

  if (N0 == 0) stop("No control units retained.")
  if (N1 == 0) stop("No treated units retained.")

  Y <- df_cohort %>%
    select(inst_id, year, y) %>%
    pivot_wider(
      names_from = year,
      values_from = y
    )

  Y <- unit_treat %>%
    select(inst_id) %>%
    left_join(Y, by = "inst_id")

  Y_mat <- Y %>%
    select(-inst_id) %>%
    as.matrix()

  storage.mode(Y_mat) <- "numeric"

  T0 <- sum(as.integer(colnames(Y_mat)) < cohort_year)

  if (anyNA(Y_mat)) {
    stop("Y contains NA after balancing. Check duplicate or missing years.")
  }

  list(
    Y = Y_mat,
    N0 = N0,
    N1 = N1,
    T0 = T0,
    unit_ids = Y$inst_id
  )
}

# ============================================================
# 3. ACE I synthetic DID
# ============================================================

panel_ace1 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2014,
  min_year = 2006,
  max_year = 2025
)

sdid_ace1 <- synthdid_estimate(
  Y = panel_ace1$Y,
  N0 = panel_ace1$N0,
  T0 = panel_ace1$T0
)

summary(sdid_ace1)

plot(
  sdid_ace1
)

# ============================================================
# 4. ACE II synthetic DID
# ============================================================

panel_ace2 <- make_sdid_panel(
  data = df_sdid,
  cohort_year = 2016,
  min_year = 2006,
  max_year = 2025
)

sdid_ace2 <- synthdid_estimate(
  Y = panel_ace2$Y,
  N0 = panel_ace2$N0,
  T0 = panel_ace2$T0
)

summary(sdid_ace2)

plot(
  sdid_ace2,
  overlay = 1,
  main = "Synthetic DID: ACE II"
)



library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(fixest)

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
# 2. Pre-treatment characteristics for matching
#    Use only variables observed before treatment
# ============================================================

pre_chars <- df_base %>%
  filter(year <= 2013) %>%
  mutate(
    log_pub = log1p(n_publication)
  ) %>%
  group_by(inst_id) %>%
  summarise(
    pub_2013 = log_pub[year == 2013][1],
    pub_slope_pre = if (sum(!is.na(log_pub)) >= 3) {
      coef(lm(log_pub ~ year))[2]
    } else {
      NA_real_
    },
    .groups = "drop"
  )

match_data <- df_base %>%
  filter(year == 2013) %>%
  select(
    inst_id,
    treated_ever,
    country_code,
    founded_date,
    gdp_cap,
    internet
  ) %>%
  left_join(pre_chars, by = "inst_id") %>%
  mutate(
    log_age_2013 = log1p(pmax(2013 - founded_date, 0))
  ) %>%
  select(
    inst_id,
    treated_ever,
    country_code,
    pub_2013,
    pub_slope_pre,
    log_age_2013,
    gdp_cap,
    internet
  ) %>%
  drop_na()

table(match_data$treated_ever)

# ============================================================
# 3. Mahalanobis matching
# ============================================================

m_match <- matchit(
  treated_ever ~
    pub_2013 +
    pub_slope_pre +
    log_age_2013 +
    gdp_cap +
    internet,
  data = match_data,
  method = "nearest",
  distance = "mahalanobis",
  ratio = 3,
  replace = FALSE
)

summary(m_match)
love.plot(m_match, threshold = 0.1)

matched_units <- match.data(m_match) %>%
  select(inst_id, weights, subclass, treated_ever)

# ============================================================
# 4. Build matched panel
# ============================================================

df_matched <- df_base %>%
  inner_join(
    matched_units,
    by = c("inst_id", "treated_ever")
  ) %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    L2_log_citations = lag(log1p(n_citations), 2),
    L2_inter_africa = lag(as.integer(n_inter_african > 0), 2),
    L2_extra_africa = lag(as.integer(n_extra_african > 0), 2)
  ) %>%
  ungroup() %>%
  mutate(
    country_year = interaction(country_code, year, drop = TRUE),
    post = as.integer(!is.na(treat_year) & year >= treat_year),
    did = treated_ever * post
  )

# ============================================================
# 5. Matched DID: average treatment effect
# ============================================================

did_match <- feols(
  log1p(n_publication) ~
    did +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_matched,
  weights = ~ weights,
  cluster = ~ country_code
)

summary(did_match)

# ============================================================
# 6. Matched event-study DID
# ============================================================

df_matched <- df_matched %>%
  mutate(
    event_time = year - treat_year
  )

event_match <- feols(
  log1p(n_publication) ~
    i(event_time, treated_ever, ref = -1) +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_matched,
  weights = ~ weights,
  cluster = ~ country_code
)

summary(event_match)

iplot(
  event_match,
  main = "Matched DID event-study: ACE programs",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)


# ============================================================
# 6. Matched event-study DID — corrected
# ============================================================

df_matched <- df_matched %>%
  mutate(
    event_time = if_else(
      treated_ever == 1,
      year - treat_year,
      -1000
    )
  )

event_match <- feols(
  log1p(n_publication) ~
    i(event_time, treated_ever, ref = -1) +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_matched,
  weights = ~ weights,
  cluster = ~ country_code
)

summary(event_match)

iplot(
  event_match,
  main = "Matched DID event-study: ACE programs",
  xlab = "Years relative to treatment",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ============================================================
# 7. Joint pre-trend test — corrected
# ============================================================

pre_coef <- names(coef(event_match))[
  grepl("event_time::-[0-9]+:treated_ever", names(coef(event_match))) &
    !grepl("event_time::-1:treated_ever", names(coef(event_match))) &
    !grepl("event_time::-1000:treated_ever", names(coef(event_match)))
]

pretrend_test <- wald(
  event_match,
  paste0(pre_coef, " = 0")
)

pretrend_test

# ============================================================
# 7. Joint pre-trend test
# ============================================================

pre_coef <- names(coef(event_match))[
  grepl("event_time::-", names(coef(event_match)) &
          !grepl("event_time::-1", names(coef(event_match)))
]

pretrend_test <- wald(
  event_match,
  paste0(pre_coef, " = 0")
)

pretrend_test

# ============================================================
# 8. Regression table
# ============================================================

etable(
  did_match,
  event_match,
  dict = c(
    did = "ACE treatment",
    L2_log_citations = "Log citations (t-2)",
    L2_inter_africa = "Inter-African collaboration (t-2)",
    L2_extra_africa = "Extra-African collaboration (t-2)"
  ),
  fitstat = ~ n + r2 + ar2,
  se.below = TRUE
)