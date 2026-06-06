library(dplyr)
library(MatchIt)
library(fixest)
library(ggplot2)
library(broom)
library(patchwork)

########################################################
# 0. Setup
########################################################

df_did <- df_africa |>
  mutate(
    log_pub = log1p(n_publication),
    ace1_group = as.integer(name %in% ace1_universities),
    ace2_group = as.integer(name %in% ace2_universities),
    post_2014 = as.integer(year >= 2014)
  )

########################################################
# 1. SPECIFICATION 1:
#    ACE1 vs all non-ACE1 universities
########################################################

df_all <- df_did |>
  mutate(
    treated = ace1_group
  )

es_all <- feols(
  log_pub ~ i(year, treated, ref = 2013) |
    name + year,
  cluster = ~country_code,
  data = df_all
)

########################################################
# 2. SPECIFICATION 2:
#    ACE1 vs ACE2 pipeline controls
########################################################

df_pipeline <- df_did |>
  filter(
    ace1_group == 1 | ace2_group == 1
  ) |>
  mutate(
    treated = ace1_group
  )

es_pipeline <- feols(
  log_pub ~ i(year, treated, ref = 2013) |
    name + year,
  cluster = ~country_code,
  data = df_pipeline
)

########################################################
# 3. SPECIFICATION 3:
#    ACE1 vs matched ACE2 controls
########################################################

pre_cov <- df_pipeline |>
  filter(year <= 2013) |>
  group_by(name, country_code) |>
  summarise(
    treated = max(treated),
    mean_pub_pre = mean(n_publication, na.rm = TRUE),
    pub_2006 = mean(n_publication[year == 2006], na.rm = TRUE),
    pub_2013 = mean(n_publication[year == 2013], na.rm = TRUE),
    pre_growth = log1p(pub_2013) - log1p(pub_2006),
    .groups = "drop"
  ) |>
  filter(
    !is.na(mean_pub_pre),
    !is.na(pre_growth)
  )

m_match <- matchit(
  treated ~ mean_pub_pre + pre_growth,
  data = pre_cov,
  method = "nearest",
  distance = "glm",
  ratio = 2,
  replace = TRUE
)

matched_units <- match.data(m_match) |>
  select(
    name,
    treated,
    weights
  )

df_matched <- df_pipeline |>
  select(-treated) |>
  inner_join(
    matched_units,
    by = "name"
  )

es_matched <- feols(
  log_pub ~ i(year, treated, ref = 2013) |
    name + year,
  weights = ~weights,
  cluster = ~country_code,
  data = df_matched
)

########################################################
# 4. Function to extract event-study coefficients
########################################################

extract_event <- function(model, spec_name) {
  
  broom::tidy(model, conf.int = TRUE) |>
    filter(grepl("year::", term)) |>
    mutate(
      event_year = as.numeric(gsub("year::", "", term)),
      specification = spec_name
    ) |>
    select(
      specification,
      event_year,
      estimate,
      conf.low,
      conf.high
    )
}

event_df <- bind_rows(
  extract_event(es_all, "All controls"),
  extract_event(es_pipeline, "ACE2 controls"),
  extract_event(es_matched, "Matched ACE2 controls")
)

########################################################
# 5. Plot the three specifications together
########################################################

p_three_specs <- ggplot(
  event_df,
  aes(
    x = event_year,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 2014,
    linetype = "dotted"
  ) +
  geom_pointrange() +
  facet_wrap(
    ~ specification,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "ACE1 event-study estimates across control groups",
    x = "Year",
    y = "Effect on log(1 + publications)"
  ) +
  theme_minimal(base_size = 13)

p_three_specs

ggsave(
  "ace1_event_study_three_specifications.jpeg",
  p_three_specs,
  width = 9,
  height = 11,
  dpi = 300
)

########################################################
# 6. Optional: DID average effects
########################################################

did_all <- feols(
  log_pub ~ treated:post_2014 |
    name + year,
  cluster = ~country_code,
  data = df_all
)

did_pipeline <- feols(
  log_pub ~ treated:post_2014 |
    name + year,
  cluster = ~country_code,
  data = df_pipeline
)

did_matched <- feols(
  log_pub ~ treated:post_2014 |
    name + year,
  weights = ~weights,
  cluster = ~country_code,
  data = df_matched
)

etable(
  did_all,
  did_pipeline,
  did_matched,
  tex = FALSE
)
