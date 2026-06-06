df <- df_africa |>
  mutate(
    # First year ACE 1 was active for this institution
    G_ace1 = case_when(
      ace_1 == 1 ~ 2014L,   # ← replace with actual cohort year if known
      TRUE       ~ 0L
    ),
    # First year ACE 2 was active
    G_ace2 = case_when(
      ace_2 == 1 ~ 2019L,   # ← replace with actual cohort year if known
      TRUE       ~ 0L
    ),
    # Combined: first time either program touched this institution
    G_any = case_when(
      G_ace1 > 0 & G_ace2 > 0 ~ pmin(G_ace1, G_ace2),
      G_ace1 > 0               ~ G_ace1,
      G_ace2 > 0               ~ G_ace2,
      TRUE                     ~ 0L
    ),
    log_pub = log1p(n_publication),
    log_gdp_cap = log(gdp_cap)
  )
 
# ── 2. Control variables ──────────────────────────────────────
controls_base  <- c("log_gdp_cap", "gdp_growth")
controls_full  <- c("log_gdp_cap", "gdp_growth", "internet",
                    "electricity", "tertiary_enrol",
                    "education_exp", "rd_exp", "researchers")
 
# ── 3. TWFE specifications (feols, clustered SEs by institution)
# ─────────────────────────────────────────────────────────────
# Spec 1: raw DiD, no controls
twfe_1 <- feols(
  log_pub ~ ace_1 + ace_2 | inst_id + year,
  data    = df %>% filter(year <=2025),
  cluster = ~inst_id
)
 
# Spec 2: + macro controls (country-level time variation)
twfe_2 <- feols(
  log_pub ~ ace_1 + ace_2 +
    log_gdp_cap + gdp_growth | inst_id + year,
  data    = df %>% filter(year <=2025),
  cluster = ~inst_id
)
 
# Spec 3: full controls
twfe_3 <- feols(
  log_pub ~ ace_1 + ace_2 +
    log_gdp_cap + gdp_growth + internet + electricity +
    tertiary_enrol + education_exp + rd_exp + researchers |
    inst_id + year,
  data    = df %>% filter(year <=2025),
  cluster = ~inst_id
)
 
# Spec 4: country × year FE instead of year FE alone
#   Absorbs country-level shocks; better parallel trends for heterogeneous countries
twfe_4 <- feols(
  log_pub ~ ace_1 + ace_2 +
    log_gdp_cap + gdp_growth + internet + electricity +
    tertiary_enrol + education_exp + rd_exp + researchers |
    inst_id + country_code^year,
  data    = df %>% filter(year <= 2025),
  cluster = ~inst_id
)
 
# ── 4. Summary table ──────────────────────────────────────────
etable(
  twfe_1, twfe_2, twfe_3, twfe_4,
  title       = "TWFE DiD: Effect of ACE 1 & ACE 2 on log(publications+1)",
  headers     = c("(1) Raw", "(2) Macro", "(3) Full", "(4) Ctry×Yr FE"),
  se.below    = TRUE,
  file        = "twfe_results.tex"   # optional LaTeX export
)


cs_any <- att_gt(
  yname         = "log_pub",
  tname         = "year",
  idname        = "inst_id_num",
  gname         = "G_any",
  xformla       = ~ log_gdp_cap + gdp_growth + internet +
                    electricity + tertiary_enrol + education_exp +
                    rd_exp + researchers,
  data          = df,
  control_group = "nevertreated",    # stricter: never-treated only
  est_method    = "dr",
  panel         = TRUE,
  allow_unbalanced_panel = TRUE,
  clustervars   = "inst_id_num"
)
 
cs_any_agg <- aggte(cs_any, type = "simple")
summary(cs_any_agg)
 
ggdid(aggte(cs_any, type = "dynamic", na.rm = TRUE),
      title = "Callaway-Sant'Anna: Any ACE dynamic ATT")

es_ace1 <- feols(
  log_pub ~ i(year, ace_1, ref = 2013) +   # year before ACE1 as ref
    log_gdp_cap + gdp_growth + internet +
    electricity + tertiary_enrol + education_exp | inst_id + country_code^year,
  data    = df,
  cluster = ~inst_id
)
 
iplot(es_ace1,
      main  = "Event study: ACE 1 → log(publications+1)",
      xlab  = "Year relative to ACE 1 launch",
      ref.line = TRUE)


# ACE 1 — ref year is 2013 (last pre-treatment year)
iplot(es_ace1,
      main     = "Event study: ACE 1",
      xlab     = "Year",
      ylab     = "ATT estimate (log publications)",
      ref.line = TRUE,        # vertical line at treatment year
      ci.lwd   = 1.5)

# What you're looking for:
# ✓ Pre-treatment coefficients (left of ref line) hover around 0
# ✓ Wide CIs pre-treatment are ok, just check point estimates
# ✗ A pre-existing upward trend = violation
# ✗ Anticipation effects (jump one year before treatment) = also a problem

df_africa_kernel <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    research_t  = log1p(n_publication),
    research_t6 = dplyr::lead(research_t, 6)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(research_t),
    !is.na(research_t6)
  )

kernel_south_t6 <- npreg(
  research_t6 ~ research_t,
  data = df_africa_kernel
)

grid_south <- data.frame(
  research_t = seq(
    min(df_africa_kernel$research_t, na.rm = TRUE),
    max(df_africa_kernel$research_t, na.rm = TRUE),
    length.out = 300
  )
)

grid_south$research_t6_hat <- predict(
  kernel_south_t6,
  newdata = grid_south
)

kernel <- ggplot(grid_south, aes(research_t, research_t6_hat)) +
  geom_line(linewidth = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Research output at t: log(1 + publications)",
    y = "Research output at t + 6",
    title = "6-year research transition function"
  ) +
  theme_minimal()

ggsave('kernel.jpeg', kernel)


library(dplyr)
library(np)
library(ggplot2)

# --------------------------------------------------
# Relative transition paths (Phillips-Sul style)
# --------------------------------------------------

df_transition <- df_africa %>%
  mutate(
    pub = log1p(n_publication)
  ) %>%
  group_by(year) %>%
  mutate(
    h_it = pub / mean(pub, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    h_t = h_it,
    h_t6 = dplyr::lead(h_it, 6)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(h_t),
    !is.na(h_t6)
  )

# --------------------------------------------------
# Non-parametric transition function
# --------------------------------------------------

kernel_h <- npreg(
  h_t6 ~ h_t,
  data = df_transition
)

# --------------------------------------------------
# Prediction grid
# --------------------------------------------------

grid <- data.frame(
  h_t = seq(
    min(df_transition$h_t, na.rm = TRUE),
    max(df_transition$h_t, na.rm = TRUE),
    length.out = 300
  )
)

grid$h_t6_hat <- predict(
  kernel_h,
  newdata = grid
)

# --------------------------------------------------
# Plot
# --------------------------------------------------

kernel_2 <- ggplot(grid, aes(x = h_t, y = h_t6_hat)) +
  geom_line(linewidth = 1.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Relative publication transition function",
    x = 'Normalized publications at t',
    y = 'Normalized publications at t+6'
  ) +
  theme_minimal(base_size = 14)

ggsave('kernel2.jpeg',kernel_2)
