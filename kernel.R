library(dplyr)
library(ggplot2)
library(patchwork)
library(furrr)

make_transition_ci_fast <- function(data, xvar, yvar, title, xlab, ylab,
                                     B = 100, span = 0.45) {

  df <- data %>%
    filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]])) %>%
    select(name, x = all_of(xvar), y = all_of(yvar))

  grid <- data.frame(
    x = seq(
      quantile(df$x, 0.01, na.rm = TRUE),
      quantile(df$x, 0.99, na.rm = TRUE),
      length.out = 200
    )
  )

  # base fit - default "interpolate" surface, much faster than "direct"
  fit <- loess(y ~ x, data = df, span = span, degree = 1)
  grid$fit <- predict(fit, newdata = grid)

  ids      <- unique(df$name)
  df_split <- split(df, df$name)  # precompute once, avoid repeated filtering

  boot_list <- future_map(seq_len(B), function(i) {
    boot_ids <- sample(ids, length(ids), replace = TRUE)
    boot_df  <- bind_rows(df_split[boot_ids])

    boot_fit <- tryCatch(
      loess(y ~ x, data = boot_df, span = span, degree = 1),
      error = function(e) NULL
    )

    if (is.null(boot_fit)) {
      rep(NA_real_, nrow(grid))
    } else {
      predict(boot_fit, newdata = grid)
    }
  }, .options = furrr_options(seed = TRUE))

  boot_mat <- do.call(cbind, boot_list)

  grid$lower <- apply(boot_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  grid$upper <- apply(boot_mat, 1, quantile, probs = 0.975, na.rm = TRUE)

  ggplot(grid, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(linewidth = 1.1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal(base_size = 12)
}

plan(multisession, workers = parallel::detectCores() - 1)

set.seed(123)
country_year_plots <- (
  make_transition_ci_fast(
    df_trans,
    "rel_log_pub_t",
    "rel_log_pub_t5",
    "Publications",
    "Position at t",
    "Position at t + 5",
    B = 500
  ) |
    make_transition_ci_fast(
      df_trans,
      "rel_log_cit_t",
      "rel_log_cit_t5",
      "Citations",
      "Position at t",
      "Position at t + 5",
      B = 500
    ) |
    make_transition_ci_fast(
      df_trans,
      "rel_log_cpp_t",
      "rel_log_cpp_t5",
      "Citations/publication",
      "Position at t",
      "Position at t + 5",
      B = 500
    )
) +
  plot_annotation(
    title = "Transition functions net of country-year mean"
  )

plan(sequential)  # reset after, optional

ggsave('country_plots_transition.jpeg', country_year_plots, width = 13, height = 6)


library(segmented)

lm1 <- lm(rel_log_cpp_t5 ~ rel_log_cpp_t, data = df_trans)

seg <- segmented(
  lm1,
  seg.Z = ~ rel_log_cpp_t,
  psi = 0.5
)

summary(seg)



library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------
# 1. Parameters
# -----------------------------

threshold <- 1.28   # replace by your estimated threshold if needed

base_year <- 2006

# -----------------------------
# 2. Prepare data
# -----------------------------

df_balboni <- df_africa %>%
  mutate(
    log_cpp = ifelse(
      n_publication > 0,
      log1p(n_citations / n_publication),
      NA_real_
    ),
    university_age = year - founded_date
  ) %>%
  filter(
    !is.na(log_cpp),
    is.finite(log_cpp),
    !is.na(university_age)
  )

# baseline position used to define above/below threshold
baseline <- df_balboni %>%
  filter(year == base_year) %>%
  group_by(country_code) %>%
  mutate(
    rel_log_cpp_base = log_cpp - mean(log_cpp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    threshold_group = ifelse(
      rel_log_cpp_base < threshold,
      "Below T",
      "Above T"
    ),
    age_group = ifelse(
      university_age < median(university_age, na.rm = TRUE),
      "Young universities",
      "Old universities"
    )
  ) %>%
  dplyr::select(name, threshold_group, age_group)

# merge baseline groups back into panel
df_balboni <- df_balboni %>%
  left_join(baseline, by = "name") %>%
  filter(
    !is.na(threshold_group),
    !is.na(age_group)
  )

# -----------------------------
# 3. Compute deciles by year/group
# -----------------------------

decile_data <- df_balboni %>%
  group_by(age_group, threshold_group, year) %>%
  summarise(
    p10 = quantile(log_cpp, 0.10, na.rm = TRUE),
    p20 = quantile(log_cpp, 0.20, na.rm = TRUE),
    p30 = quantile(log_cpp, 0.30, na.rm = TRUE),
    p40 = quantile(log_cpp, 0.40, na.rm = TRUE),
    p50 = quantile(log_cpp, 0.50, na.rm = TRUE),
    p60 = quantile(log_cpp, 0.60, na.rm = TRUE),
    p70 = quantile(log_cpp, 0.70, na.rm = TRUE),
    p80 = quantile(log_cpp, 0.80, na.rm = TRUE),
    p90 = quantile(log_cpp, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "percentile",
    values_to = "value"
  )

# -----------------------------
# 4. Plot Balboni-style figure
# -----------------------------

p_balboni <- ggplot(
  decile_data,
  aes(
    x = year,
    y = value,
    group = percentile,
    linetype = percentile
  )
) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = threshold,
    linewidth = 0.6
  ) +
  facet_grid(
    age_group ~ threshold_group
  ) +
  labs(
    title = "Citation-intensity dynamics above and below threshold",
    x = "Year",
    y = "log(1 + citations per publication)",
    linetype = "Percentile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_balboni

ggsave(
  "balboni_style_citation_intensity.jpeg",
  p_balboni,
  width = 11,
  height = 9,
  dpi = 300
)





library(dplyr)
library(ggplot2)
library(patchwork)

# --------------------------------------------------
# 1. Build cohort transition panel
# --------------------------------------------------

df_cohort <- df_africa %>%
  mutate(
    log_cpp = ifelse(
      n_publication > 0,
      log1p(n_citations / n_publication),
      NA_real_
    )
  ) %>%
  filter(
    !is.na(log_cpp),
    is.finite(log_cpp),
    !is.na(founded_date)
  ) %>%
  mutate(
    cohort = case_when(
      founded_date < 1960 ~ "Before 1960",
      founded_date >= 1960 & founded_date < 1980 ~ "1960–1979",
      founded_date >= 1980 & founded_date < 2000 ~ "1980–1999",
      founded_date >= 2000 ~ "2000+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(country_code, year) %>%
  mutate(
    rel_log_cpp = log_cpp - mean(log_cpp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(
    rel_log_cpp_t = rel_log_cpp,
    rel_log_cpp_t5 = dplyr::lead(rel_log_cpp, 5),
    year_t5 = dplyr::lead(year, 5)
  ) %>%
  ungroup() %>%
  filter(
    year_t5 == year + 5,
    !is.na(rel_log_cpp_t),
    !is.na(rel_log_cpp_t5),
    !is.na(cohort)
  )

# --------------------------------------------------
# 2. Fast LOESS transition function by cohort
# --------------------------------------------------

make_cohort_transition <- function(data, span = 0.45) {
  
  grid_data <- data %>%
    group_by(cohort) %>%
    group_modify(~ {
      
      df <- .x
      
      grid <- data.frame(
        rel_log_cpp_t = seq(
          quantile(df$rel_log_cpp_t, 0.01, na.rm = TRUE),
          quantile(df$rel_log_cpp_t, 0.99, na.rm = TRUE),
          length.out = 200
        )
      )
      
      fit <- loess(
        rel_log_cpp_t5 ~ rel_log_cpp_t,
        data = df,
        span = span,
        degree = 1,
        control = loess.control(surface = "direct")
      )
      
      grid$fit <- predict(fit, newdata = grid)
      grid
    }) %>%
    ungroup()
  
  ggplot(
    grid_data,
    aes(x = rel_log_cpp_t, y = fit)
  ) +
    geom_line(linewidth = 1.1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    facet_wrap(~ cohort, ncol = 2) +
    labs(
      title = "Citation-intensity transition functions by founding cohort",
      x = "Position at t within country-year",
      y = "Expected position at t + 5 within country-year"
    ) +
    theme_minimal(base_size = 13)
}

p_cohort_transition <- make_cohort_transition(df_cohort)

p_cohort_transition

ggsave(
  "cohort_transition_citation_intensity.jpeg",
  p_cohort_transition,
  width = 11,
  height = 8,
  dpi = 300
)

# --------------------------------------------------
# 3. Mobility functions by cohort
# --------------------------------------------------

df_cohort <- df_cohort %>%
  mutate(
    d_rel_log_cpp = rel_log_cpp_t5 - rel_log_cpp_t
  )

make_cohort_mobility <- function(data, span = 0.45) {
  
  grid_data <- data %>%
    group_by(cohort) %>%
    group_modify(~ {
      
      df <- .x
      
      grid <- data.frame(
        rel_log_cpp_t = seq(
          quantile(df$rel_log_cpp_t, 0.01, na.rm = TRUE),
          quantile(df$rel_log_cpp_t, 0.99, na.rm = TRUE),
          length.out = 200
        )
      )
      
      fit <- loess(
        d_rel_log_cpp ~ rel_log_cpp_t,
        data = df,
        span = span,
        degree = 1,
        control = loess.control(surface = "direct")
      )
      
      grid$fit <- predict(fit, newdata = grid)
      grid
    }) %>%
    ungroup()
  
  ggplot(
    grid_data,
    aes(x = rel_log_cpp_t, y = fit)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ cohort, ncol = 2) +
    labs(
      title = "Citation-intensity mobility functions by founding cohort",
      x = "Position at t within country-year",
      y = "Change in position, t to t + 5"
    ) +
    theme_minimal(base_size = 13)
}

p_cohort_mobility <- make_cohort_mobility(df_cohort)

p_cohort_mobility

ggsave(
  "cohort_mobility_citation_intensity.jpeg",
  p_cohort_mobility,
  width = 11,
  height = 8,
  dpi = 300
)

# --------------------------------------------------
# 4. Optional: sample size by cohort
# --------------------------------------------------

df_cohort %>%
  count(cohort)






library(dplyr)
library(tidyr)
library(ggplot2)

# ==================================================
# Balboni-style specification:
# distance to national citation-intensity frontier
# ==================================================

# 1. Parameters
base_year <- 2006
end_year  <- 2020          # avoids truncated citation window after 2020
frontier_p <- 0.90         # national frontier = p90 within country-year
age_cut <- NULL            # if NULL, median founding year is used

# Use your estimated threshold if available
threshold <- 1.28

# 2. Build variable
df_balb <- df_africa %>%
  mutate(
    cpp = ifelse(
      n_publication > 0,
      n_citations / n_publication,
      NA_real_
    ),
    log_cpp = log1p(cpp)
  ) %>%
  filter(
    year >= base_year,
    year <= end_year,
    !is.na(log_cpp),
    is.finite(log_cpp),
    !is.na(founded_date),
    !is.na(country_code),
    !is.na(name)
  )

# 3. Compute national frontier and distance to frontier
df_balb <- df_balb %>%
  group_by(country_code, year) %>%
  mutate(
    n_country_year = n(),
    frontier_cpp = quantile(
      log_cpp,
      probs = frontier_p,
      na.rm = TRUE
    ),
    dist_frontier = frontier_cpp - log_cpp
  ) %>%
  ungroup() %>%
  filter(
    n_country_year >= 5
  )

# 4. Define baseline threshold status and founding cohort
baseline_groups <- df_balb %>%
  filter(year == base_year) %>%
  group_by(country_code) %>%
  mutate(
    rel_log_cpp_base =
      log_cpp - mean(log_cpp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    threshold_group = ifelse(
      rel_log_cpp_base >= threshold,
      "Above T",
      "Below T"
    )
  ) %>%
  dplyr::select(
    name,
    threshold_group
  )

if (is.null(age_cut)) {
  age_cut <- median(df_balb$founded_date, na.rm = TRUE)
}

cohort_groups <- df_balb %>%
  distinct(name, founded_date) %>%
  mutate(
    cohort_group = ifelse(
      founded_date <= age_cut,
      "Old universities",
      "Young universities"
    )
  ) %>%
  dplyr::select(
    name,
    cohort_group
  )

df_balb <- df_balb %>%
  left_join(baseline_groups, by = "name") %>%
  left_join(cohort_groups, by = "name") %>%
  filter(
    !is.na(threshold_group),
    !is.na(cohort_group)
  )

# 5. Compute decile trajectories
decile_data <- df_balb %>%
  group_by(
    cohort_group,
    threshold_group,
    year
  ) %>%
  summarise(
    p10 = quantile(dist_frontier, 0.10, na.rm = TRUE),
    p20 = quantile(dist_frontier, 0.20, na.rm = TRUE),
    p30 = quantile(dist_frontier, 0.30, na.rm = TRUE),
    p40 = quantile(dist_frontier, 0.40, na.rm = TRUE),
    p50 = quantile(dist_frontier, 0.50, na.rm = TRUE),
    p60 = quantile(dist_frontier, 0.60, na.rm = TRUE),
    p70 = quantile(dist_frontier, 0.70, na.rm = TRUE),
    p80 = quantile(dist_frontier, 0.80, na.rm = TRUE),
    p90 = quantile(dist_frontier, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "percentile",
    values_to = "value"
  ) %>%
  mutate(
    percentile = factor(
      percentile,
      levels = paste0("p", seq(10, 90, 10))
    ),
    threshold_group = factor(
      threshold_group,
      levels = c("Below T", "Above T")
    ),
    cohort_group = factor(
      cohort_group,
      levels = c("Young universities", "Old universities")
    )
  )

# 6. Plot
p_balb <- ggplot(
  decile_data,
  aes(
    x = year,
    y = value,
    group = percentile,
    linetype = percentile
  )
) +
  geom_line(linewidth = 0.75) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.6
  ) +
  facet_grid(
    cohort_group ~ threshold_group
  ) +
  labs(
    title = "Distance-to-frontier dynamics above and below citation-intensity threshold",
    x = "Year",
    y = "Distance to national citation-intensity frontier",
    linetype = "Percentile"
  ) +
  scale_x_continuous(
    breaks = seq(base_year, end_year, by = 2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

p_balb

ggsave(
  "balboni_style_distance_to_frontier.jpeg",
  p_balb,
  width = 11,
  height = 8,
  dpi = 300
)


# 7. Diagnostics
df_balb %>%
  count(cohort_group, threshold_group)

decile_data %>%
  group_by(cohort_group, threshold_group) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    .groups = "drop"
  )



ggplot(
  decile_data,
  aes(
    x = year,
    y = value,
    group = percentile,
    color = percentile,
    linetype = percentile
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.6,
    color = "black"
  ) +
  facet_grid(
    cohort_group ~ threshold_group
  ) +
  scale_color_viridis_d(

  option = "plasma",

  end = 0.9

) +
  labs(
    title = "Distance-to-frontier dynamics above and below citation-intensity threshold",
    x = "Year",
    y = "Distance to national citation-intensity frontier",
    color = "Percentile",
    linetype = "Percentile"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )



library(mgcv)

gam_fit <- gam(
  rel_log_cpp_t5 ~ s(rel_log_cpp_t),
  data = df_trans
)

summary(gam_fit)
plot(gam_fit)

ggsave(
  filename = "balboni_distance_frontier.jpeg",
  plot = p_balb,
  width = 14,
  height = 10,
  units = "in",
  dpi = 600
)

jpeg(
  "gam_transition_plot.jpeg",
  width = 1800,
  height = 1200,
  res = 300
)

plot(
  gam_fit,
  shade = TRUE,
  shade.col = "lightgrey",
  seWithMean = TRUE
)

dev.off()


library(gratia)
library(ggplot2)

p <- draw(gam_fit) +
  labs(
    x = "Relative citation intensity at t",
    y = "Estimated smooth effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    axis.title = element_text(face = "bold"),
    plot.title = element_blank()
  )

ggsave(
  "gam_publication.jpeg",
  p,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white"
)

jpeg(
  "gam_publication.jpeg",
  width = 1800,
  height = 1200,
  res = 300,
  quality = 100
)

par(
  mar = c(4,4,1,1),   # bottom,left,top,right
  mgp = c(2.2,0.7,0),
  xaxs = "i",
  yaxs = "i"
)

plot(
  gam_fit,
  residuals = FALSE,
  rug = TRUE,
  shade = TRUE,
  shade.col = "grey85",
  seWithMean = TRUE,
  lwd = 2,
  cex.lab = 1.3,
  cex.axis = 1.1
)

dev.off()



threshold <- 1.28

jpeg(
  "gam_threshold.jpeg",
  width = 1800,
  height = 1200,
  res = 300
)

plot(
  gam_fit,
  shade = TRUE,
  shade.col = "grey85",
  residuals = FALSE,
  rug = TRUE
)

abline(
  v = threshold,
  col = "red",
  lwd = 1,
  lty = 1
)

dev.off()

library(lmtest)

bptest(seg$lm.fit)

bptest(seg$lm.fit)
