pretrend <- df_africa |>
  mutate(
    ace1_group = name %in% ace1_universities,
    ace2_group = name %in% ace2_universities
  ) |>
  filter(year <= 2013) |>
  mutate(
    group = case_when(
      ace1_group ~ "ACE1",
      ace2_group ~ "ACE2",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(group)) |>
  group_by(year, group) |>
  summarise(
    mean_pub = mean(log1p(n_publication), na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  pretrend,
  aes(x = year, y = mean_pub, color = group)
) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean log(1 + publications)",
    color = "Group"
  )

df_pre <- df_africa |>

  mutate(

    ace1_group = as.integer(name %in% ace1_universities),

    ace2_group = as.integer(name %in% ace2_universities)

  ) |>

  filter(

    year <= 2013,

    ace1_group == 1 | ace2_group == 1

  )

pretrend_test <- feols(

  log1p(n_publication) ~

    ace1_group:year |

    name + year,

  cluster = ~country_code,

  data = df_pre)




summary(pretrend_test)


########################################################
# PROPENSITY SCORE MATCHING + DID
########################################################

library(dplyr)
library(MatchIt)
library(cobalt)
library(fixest)
library(ggplot2)
library(ggfixest)

########################################################
# 1. Define treatment groups
########################################################

df_africa <- df_africa |>
  mutate(
    ace1_group = as.integer(name %in% ace1_universities),
    ace2_group = as.integer(name %in% ace2_universities),
    any_ace = as.integer(ace1_group == 1 | ace2_group == 1)
  )

########################################################
# 2. Build pre-treatment university-level covariates
#    Use only pre-ACE1 years: 2006–2013
########################################################

pre_cov <- df_africa |>
  filter(year <= 2013) |>
  group_by(name, country_code) |>
  summarise(
    ace1_group = max(ace1_group, na.rm = TRUE),
    ace2_group = max(ace2_group, na.rm = TRUE),

    mean_pub_pre = mean(n_publication, na.rm = TRUE),
    log_mean_pub_pre = log1p(mean_pub_pre),

    pub_2006 = mean(n_publication[year == 2006], na.rm = TRUE),
    pub_2013 = mean(n_publication[year == 2013], na.rm = TRUE),
    log_pub_2006 = log1p(pub_2006),
    log_pub_2013 = log1p(pub_2013),

    pre_growth = log1p(pub_2013) - log1p(pub_2006),

    mean_gdp_pre = mean(gdp_cap, na.rm = TRUE),
    mean_net_pre = mean(net_us, na.rm = TRUE),
    mean_el_pre = mean(el_acc.x, na.rm = TRUE),

    .groups = "drop"
  ) |>
  mutate(
    mean_gdp_pre = ifelse(is.nan(mean_gdp_pre), NA, mean_gdp_pre),
    mean_net_pre = ifelse(is.nan(mean_net_pre), NA, mean_net_pre),
    mean_el_pre = ifelse(is.nan(mean_el_pre), NA, mean_el_pre)
  )

########################################################
# 3. Choose comparison sample
#    Option A: ACE1 treated vs ACE2 pipeline controls
########################################################

match_data <- pre_cov |>
  filter(ace1_group == 1 | ace2_group == 1) |>
  mutate(
    treated = ace1_group
  ) |>
  filter(
    !is.na(log_mean_pub_pre),
    !is.na(pre_growth),
    !is.na(mean_gdp_pre),
    !is.na(mean_net_pre)
  )

########################################################
# 4. Estimate propensity score and match
########################################################

m_match <- matchit(
  treated ~
    log_mean_pub_pre +
    pre_growth +
    mean_gdp_pre +
    mean_net_pre,
  data = match_data,
  method = "nearest",
  distance = "glm",
  ratio = 1,
  replace = TRUE,
  caliper = 0.25
)

summary(m_match)

########################################################
# 5. Balance diagnostics
########################################################

love.plot(
  m_match,
  threshold = 0.1,
  abs = TRUE
)

matched_units <- match.data(m_match) |>
  dplyr::select(name, treated, weights)

########################################################
# 6. Rebuild matched panel
########################################################

df_matched <- df_africa |>
  inner_join(
    matched_units,
    by = "name"
  ) |>
  mutate(
    post_ace1 = as.integer(year >= 2014),
    log_pub = log1p(n_publication)
  )

########################################################
# 7. DID on matched sample
########################################################

did_matched <- feols(
  log_pub ~ treated:post_ace1 |
    name + year,
  weights = ~weights,
  cluster = ~country_code,
  data = df_matched
)

summary(did_matched)

########################################################
# 8. Event study on matched sample
########################################################

es_matched <- feols(
  log_pub ~ i(year, treated, ref = 2013) |
    name + year,
  weights = ~weights,
  cluster = ~country_code,
  data = df_matched
)

iplot(es_matched)

########################################################
# 9. Publication-quality event-study plot
########################################################

p_es_matched <- ggiplot(es_matched) +
  labs(
    title = "ACE1 effect using propensity-score matched controls",
    subtitle = "Treated: ACE1 universities; controls: matched ACE2 universities",
    x = "Year",
    y = "Effect on log(1 + publications)"
  ) +
  theme_minimal(base_size = 14)

p_es_matched

ggsave(
  "event_study_ace1_matched.jpeg",
  p_es_matched,
  width = 9,
  height = 6,
  dpi = 300
)

########################################################
# 10. Formal pre-trend test on matched sample
########################################################

pretrend_matched <- feols(
  log_pub ~ treated:year |
    name + year,
  weights = ~weights,
  cluster = ~country_code,
  data = df_matched |> filter(year <= 2013)
)

summary(pretrend_matched)