library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# ------------------------------------------------------------
# 1. Build categorical variables
# ------------------------------------------------------------

df_mca <- df_africa %>%
  filter(year == 2025) %>%
  mutate(
    pub_cat = ntile(n_publication, 3),
    intl_cat = ntile(international_share_any, 3),
    cit_cat = ntile(n_citations, 3),
    inter_af_cat = ntile(inter_african_share, 3),
    extra_af_cat = ntile(extra_african_share, 3)
  ) %>%
  mutate(
    across(
      ends_with("_cat"),
      ~ factor(
        .,
        levels = c(1, 2, 3),
        labels = c("Low", "Medium", "High")
      )
    )
  ) %>%
  dplyr::select(
    name,
    cit_cat,
    country_code,
    pub_cat,
    intl_cat,
    inter_af_cat,
    extra_af_cat
  ) %>%
  tidyr::drop_na()

# ------------------------------------------------------------
# 2. MCA
# ------------------------------------------------------------

mca_data <- df_mca %>%
  select(-name, -country_code)

mca_res <- MCA(
  mca_data,
  graph = FALSE
)

# ------------------------------------------------------------
# 3. Plot modalities
# ------------------------------------------------------------

p_modalities <- fviz_mca_var(
  mca_res,
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = c("grey70", "#2C7BB6", "#D7191C")
) +
  labs(
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal(base_size = 13)

p_modalities

ggsave(
  "mca_modalities_2025_cit.jpeg",
  p_modalities,
  width = 9,
  height = 7,
  dpi = 500
)

# ------------------------------------------------------------
# 4. Plot individuals: universities
# ------------------------------------------------------------

p_individuals <- fviz_mca_ind(
  mca_res,
  geom = "point",
  habillage = df_mca$country_code,
  addEllipses = FALSE,
  repel = FALSE,
  alpha.ind = 0.55
) +
  labs(
    title = "MCA individual plot",
    subtitle = "Universities positioned by bibliometric and collaboration profiles, 2025",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_individuals

ggsave(
  "mca_individuals_2025.jpeg",
  p_individuals,
  width = 9,
  height = 7,
  dpi = 500
)








library(dplyr)
library(tidyr)
library(fixest)

df_model_cit <- df_africa %>%
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

c1 <- fepois(
  n_citations ~ ace_1 + ace_2 |
    inst_id + year,
  data = df_model_cit,
  cluster = ~ country_code
)

c2 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications |
    inst_id + year,
  data = df_model_cit,
  cluster = ~ country_code
)

c3 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations |
    inst_id + year,
  data = df_model_cit,
  cluster = ~ country_code
)

c4 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations +
    L2_international |
    inst_id + year,
  data = df_model_cit,
  cluster = ~ country_code
)

c5 <- fepois(
  n_citations ~ ace_1 + ace_2 +
    L2_log_publications +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + year,
  data = df_model_cit,
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
  data = df_model_cit,
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
  file = "ppml_citations_specifications.tex"
)




library(fixest)

# ------------------------------------------------------------
# ACE I
# ------------------------------------------------------------

did_ace1 <- feols(
  log1p(n_publication) ~
    i(year, ace_1, ref = 2013) |
    inst_id + year,
  cluster = ~country_code,
  data = df_africa
)

iplot(
  did_ace1,
  main = "ACE I: Dynamic treatment effects",
  xlab = "Year",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

summary(did_ace1)


# ------------------------------------------------------------
# ACE II
# ------------------------------------------------------------

did_ace2 <- feols(
  log1p(n_publication) ~
    i(year, ace_2, ref = 2015) |
    inst_id + year,
  cluster = ~country_code,
  data = df_africa
)

iplot(
  did_ace2,
  main = "ACE II: Dynamic treatment effects",
  xlab = "Year",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

library(dplyr)
library(fixest)

# ------------------------------------------------------------
# 1. Main ACE I event-study DID
# ------------------------------------------------------------

df_did <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

did_ace1 <- feols(
  log1p(n_publication) ~
    i(year, ace_1, ref = 2013) |
    inst_id + year,
  cluster = ~country_code,
  data = df_did
)

summary(did_ace1)

iplot(
  did_ace1,
  main = "ACE I: Dynamic treatment effects",
  xlab = "Year",
  ylab = "Effect on log(publications + 1)",
  ref.line = 0,
  ci_level = 0.95
)

# ------------------------------------------------------------
# 2. Placebo / pre-trend test
# ------------------------------------------------------------

placebo_ace1 <- feols(
  log1p(n_publication) ~
    i(year, ace1_ever, ref = 2013) |
    inst_id + year,
  cluster = ~country_code,
  data = df_did
)

summary(placebo_ace1)

iplot(
  placebo_ace1,
  main = "ACE I: placebo pre-trend test",
  xlab = "Year",
  ylab = "Effect relative to 2013",
  ref.line = 0,
  ci_level = 0.95
)

# ------------------------------------------------------------
# 3. Joint test of pre-treatment coefficients
# ------------------------------------------------------------

wald(
  placebo_ace1,
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

fitstat(placebo_ace1, "wald")














wald(
  placebo_ace1,
  keep = "year::20(06|07|08|09|10|11|12)"
)




library(fixest)

# ------------------------------------------------------------
# ACE I
# ------------------------------------------------------------

did_ppml_ace1 <- fepois(
  n_publication ~
    i(year, ace_1, ref = 2013) |
    inst_id + year,
  data = df_africa,
  cluster = ~country_code
)

iplot(
  did_ppml_ace1,
  ref.line = 0,
  ci_level = 0.95,
  main = "ACE I: PPML Event Study",
  xlab = "Year",
  ylab = "Log incidence-rate ratio"
)

# ------------------------------------------------------------
# ACE II
# ------------------------------------------------------------

did_ppml_ace2 <- fepois(
  n_publication ~
    i(year, ace_2, ref = 2015) |
    inst_id + year,
  data = df_africa,
  cluster = ~country_code
)

iplot(
  did_ppml_ace2,
  ref.line = 0,
  ci_level = 0.95,
  main = "ACE II: PPML Event Study",
  xlab = "Year",
  ylab = "Log incidence-rate ratio"
)


did_placebo <- fepois(
  n_publication ~
    i(year, ace_1, ref = 2010) |
    inst_id + year,
  data = df_africa %>%
    filter(year <= 2013),
  cluster = ~country_code
)

iplot(
  did_placebo,
  main = "Placebo test: Fake treatment in 2010"
)


library(dplyr)
library(fixest)

df_placebo_ace1 <- df_africa %>%
  filter(year <= 2013) %>%
  filter(!is.na(n_publication), !is.na(inst_id), !is.na(year), !is.na(country_code)) %>%
  group_by(inst_id) %>%
  filter(sum(n_publication, na.rm = TRUE) > 0) %>%   # remove all-zero units
  filter(n_distinct(year) >= 2) %>%                  # remove singletons
  ungroup()

did_placebo <- fepois(
  n_publication ~ i(year, ace_1, ref = 2010) |
    inst_id + year,
  data = df_placebo_ace1,
  vcov = ~country_code
)

iplot(
  did_placebo,
  main = "ACE I placebo test: pre-treatment period only",
  xlab = "Year",
  ylab = "Placebo PPML coefficient"
)


library(fixest)
df_placebo_ace1 <- df_africa %>%
  filter(year <= 2013) %>%
  mutate(
    placebo_post = as.integer(year >= 2011),
    placebo_did = ace_1 * placebo_post
  ) %>%
  filter(!is.na(n_publication), !is.na(inst_id), !is.na(year), !is.na(country_code)) %>%
  group_by(inst_id) %>%
  filter(sum(n_publication, na.rm = TRUE) > 0) %>%
  filter(n_distinct(year) >= 2) %>%
  ungroup()

placebo_model <- fepois(
  n_publication ~ placebo_did |
    inst_id + year,
  data = df_placebo_ace1,
  vcov = ~country_code
)

summary(placebo_model)

placebo_model <- fixest::fepois(
  n_publication ~ placebo_did |
    inst_id + year,
  data = df_placebo_ace1,
  vcov = ~country_code
)



library(dplyr)
library(fixest)

df_placebo_ace1 <- df_africa %>%
  filter(year <= 2013) %>%
  mutate(
    placebo_post = as.integer(year >= 2011),
    placebo_did  = ace_1 * placebo_post
  ) %>%
  filter(
    !is.na(n_publication),
    !is.na(placebo_did),
    !is.na(inst_id),
    !is.na(year),
    !is.na(country_code)
  ) %>%
  group_by(inst_id) %>%
  filter(n_distinct(year) >= 2) %>%
  filter(sum(n_publication, na.rm = TRUE) > 0) %>%
  ungroup()

placebo_model <- fixest::feglm(
  n_publication ~ placebo_did | inst_id + year,
  data   = df_placebo_ace1,
  family = poisson(),
  vcov   = ~ country_code
)

summary(placebo_model)



library(dplyr)
library(fixest)

df_placebo_ace1 <- df_africa %>%
  filter(year <= 2013) %>%
  filter(
    !is.na(n_publication),
    !is.na(ace_1),
    !is.na(inst_id),
    !is.na(year),
    !is.na(country_code)
  ) %>%
  group_by(inst_id) %>%
  filter(n_distinct(year) >= 2) %>%
  filter(sum(n_publication, na.rm = TRUE) > 0) %>%
  ungroup()

placebo_es <- fixest::feglm(
  n_publication ~ i(year, ace_1, ref = 2010) |
    inst_id + year,
  data = df_placebo_ace1,
  family = poisson(),
  vcov = ~ country_code
)

summary(placebo_es)

iplot(
  placebo_es,
  main = "ACE I placebo / pre-trend test",
  xlab = "Year",
  ylab = "PPML coefficient relative to 2010"
)



library(dplyr)
library(fixest)

# ------------------------------------------------------------
# 1. Create ever-treated indicators
# ------------------------------------------------------------

df_did <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE)),
    ace2_ever = as.integer(any(ace_2 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 2. ACE I placebo: only pre-treatment years
# ------------------------------------------------------------

placebo_ace1 <- df_did %>%
  filter(year <= 2013) %>%
  filter(!is.na(n_publication), !is.na(inst_id), !is.na(year), !is.na(country_code))

m_placebo_ace1 <- feols(
  log1p(n_publication) ~ i(year, ace1_ever, ref = 2010) |
    inst_id + year,
  data = placebo_ace1,
  cluster = ~ country_code
)

iplot(
  m_placebo_ace1,
  main = "ACE I placebo pre-trend test",
  xlab = "Year",
  ylab = "Effect relative to 2010"
)

# ------------------------------------------------------------
# 3. ACE II placebo: only pre-treatment years
# ------------------------------------------------------------

placebo_ace2 <- df_did %>%
  filter(year <= 2015) %>%
  filter(!is.na(n_publication), !is.na(inst_id), !is.na(year), !is.na(country_code))

m_placebo_ace2 <- feols(
  log1p(n_publication) ~ i(year, ace2_ever, ref = 2012) |
    inst_id + year,
  data = placebo_ace2,
  cluster = ~ country_code
)

iplot(
  m_placebo_ace2,
  main = "ACE II placebo pre-trend test",
  xlab = "Year",
  ylab = "Effect relative to 2012"
)



summary(feols(
  log1p(n_publication) ~
    i(year, ace1_ever, ref = 2013) |
    inst_id + year,
  data = df_did
))








library(dplyr)
library(fixest)
library(ggplot2)
library(purrr)

# ------------------------------------------------------------
# 1. Create ever-treated ACE I indicator
# ------------------------------------------------------------

df_did <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 2. True DID estimate
# ------------------------------------------------------------

true_model <- feols(
  log1p(n_publication) ~ ace_1 |
    inst_id + year,
  data = df_did,
  cluster = ~ country_code
)

true_beta <- coef(true_model)["ace_1"]

# ------------------------------------------------------------
# 3. Placebo randomization function
# ------------------------------------------------------------

run_placebo <- function(seed) {
  
  set.seed(seed)
  
  treated_ids <- df_did %>%
    distinct(inst_id, ace1_ever) %>%
    filter(ace1_ever == 1) %>%
    pull(inst_id)
  
  all_ids <- df_did %>%
    distinct(inst_id) %>%
    pull(inst_id)
  
  fake_treated_ids <- sample(
    all_ids,
    size = length(treated_ids),
    replace = FALSE
  )
  
  df_fake <- df_did %>%
    mutate(
      fake_ever = as.integer(inst_id %in% fake_treated_ids),
      fake_ace1 = fake_ever * as.integer(year >= 2014)
    )
  
  m_fake <- feols(
    log1p(n_publication) ~ fake_ace1 |
      inst_id + year,
    data = df_fake,
    cluster = ~ country_code,
    warn = FALSE,
    notes = FALSE
  )
  
  coef(m_fake)["fake_ace1"]
}

# ------------------------------------------------------------
# 4. Run placebo simulations
# ------------------------------------------------------------

set.seed(123)

n_sim <- 500

placebo_betas <- map_dbl(1:n_sim, run_placebo)

placebo_df <- tibble(
  beta = placebo_betas
)

# ------------------------------------------------------------
# 5. Placebo p-value
# ------------------------------------------------------------

placebo_p_value <- mean(abs(placebo_df$beta) >= abs(true_beta), na.rm = TRUE)

placebo_p_value

# ------------------------------------------------------------
# 6. Plot placebo distribution
# ------------------------------------------------------------

p_placebo <- ggplot(placebo_df, aes(x = beta)) +
  geom_histogram(bins = 40, fill = "grey80", color = "white") +
  geom_vline(
    xintercept = true_beta,
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Placebo test: random ACE I assignment",
    subtitle = paste0(
      "True estimate = ", round(true_beta, 3),
      "; placebo p-value = ", round(placebo_p_value, 3)
    ),
    x = "Placebo DID coefficient",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)

p_placebo

ggsave(
  "placebo_test_ace1.jpeg",
  p_placebo,
  width = 9,
  height = 6,
  dpi = 500
)



library(dplyr)
library(fixest)

# ------------------------------------------------------------
# 1. Ever-treated indicator
# ------------------------------------------------------------

df_placebo <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 2. Restrict to pre-treatment years
# ------------------------------------------------------------

df_placebo <- df_placebo %>%
  filter(year <= 2013)

# ------------------------------------------------------------
# 3. Fake treatment beginning in 2010
# ------------------------------------------------------------

df_placebo <- df_placebo %>%
  mutate(
    placebo_post = as.integer(year >= 2010),
    placebo_did = ace1_ever * placebo_post
  )

# ------------------------------------------------------------
# 4. Estimate placebo DID
# ------------------------------------------------------------

m_placebo <- feols(
  log1p(n_publication) ~
    placebo_did |
    inst_id + year,
  data = df_placebo,
  cluster = ~country_code
)

summary(m_placebo)







library(dplyr)
library(fixest)
library(ggplot2)
library(purrr)

# ============================================================
# 1. Prepare data
# ============================================================

df_did <- df_africa %>%
  group_by(inst_id) %>%
  mutate(
    ace1_ever = as.integer(any(ace_1 == 1, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  filter(
    !is.na(n_publication),
    !is.na(inst_id),
    !is.na(year),
    !is.na(country_code)
  )

# ============================================================
# 2. Main DID
# ============================================================

did_main <- feols(
  log1p(n_publication) ~ ace_1 |
    inst_id + year,
  data = df_did,
  cluster = ~ country_code
)

summary(did_main)

true_beta <- coef(did_main)["ace_1"]

# ============================================================
# 3. Event-study DID
# ============================================================

did_event <- feols(
  log1p(n_publication) ~
    i(year, ace1_ever, ref = 2013) |
    inst_id + year,
  data = df_did,
  cluster = ~ country_code
)

summary(did_event)

iplot(
  did_event,
  main = "ACE I event-study DID",
  xlab = "Year",
  ylab = "Effect relative to 2013"
)

# ============================================================
# 4. Pre-trend joint test
# ============================================================

pretrend_test <- wald(
  did_event,
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

pretrend_test

# ============================================================
# 5. Country-stratified placebo randomization
# ============================================================

treated_by_country <- df_did %>%
  distinct(inst_id, country_code, ace1_ever) %>%
  group_by(country_code) %>%
  summarise(
    n_treated = sum(ace1_ever == 1),
    .groups = "drop"
  ) %>%
  filter(n_treated > 0)

universities_by_country <- df_did %>%
  distinct(inst_id, country_code)

draw_fake_treatment <- function(seed) {
  
  set.seed(seed)
  
  fake_ids <- treated_by_country %>%
    left_join(universities_by_country, by = "country_code") %>%
    group_by(country_code) %>%
    summarise(
      fake_inst_id = list(
        sample(
          unique(inst_id),
          size = first(n_treated),
          replace = FALSE
        )
      ),
      .groups = "drop"
    ) %>%
    tidyr::unnest(fake_inst_id) %>%
    rename(inst_id = fake_inst_id) %>%
    mutate(fake_ever = 1L)
  
  df_did %>%
    left_join(fake_ids, by = "inst_id") %>%
    mutate(
      fake_ever = if_else(is.na(fake_ever), 0L, fake_ever),
      fake_ace1 = fake_ever * as.integer(year >= 2014)
    )
}

run_placebo <- function(seed) {
  
  df_fake <- draw_fake_treatment(seed)
  
  m <- tryCatch(
    feols(
      log1p(n_publication) ~ fake_ace1 |
        inst_id + year,
      data = df_fake,
      cluster = ~ country_code,
      warn = FALSE,
      notes = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(m)) return(NA_real_)
  
  beta <- coef(m)["fake_ace1"]
  
  if (is.na(beta)) return(NA_real_)
  
  as.numeric(beta)
}

# ============================================================
# 6. Run placebo simulations
# ============================================================

set.seed(123)

n_sim <- 500

placebo_betas <- map_dbl(1:n_sim, run_placebo)

placebo_df <- tibble(
  beta = placebo_betas
) %>%
  filter(!is.na(beta))

placebo_p_value <- mean(
  abs(placebo_df$beta) >= abs(true_beta),
  na.rm = TRUE
)

placebo_p_value

# ============================================================
# 7. Plot placebo distribution
# ============================================================

p_placebo <- ggplot(placebo_df, aes(x = beta)) +
  geom_histogram(
    bins = 40,
    fill = "grey80",
    color = "white"
  ) +
  geom_vline(
    xintercept = true_beta,
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Country-stratified placebo test: ACE I",
    subtitle = paste0(
      "True estimate = ",
      round(true_beta, 3),
      "; placebo p-value = ",
      round(placebo_p_value, 3)
    ),
    x = "Placebo DID coefficient",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)

p_placebo

ggsave(
  "ace1_country_stratified_placebo.jpeg",
  p_placebo,
  width = 9,
  height = 6,
  dpi = 500
)