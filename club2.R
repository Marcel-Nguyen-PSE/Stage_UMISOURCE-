library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ConvergenceClubs)

# ============================================================
# 1. Metrics to test
# ============================================================

metrics <- c(
  "n_publication",
  "n_citations",
  "international_share_any",
  "inter_african_share",
  "extra_african_share"
)

# ============================================================
# 2. Helper: prepare wide matrix for one metric
# ============================================================

prepare_metric_matrix <- function(data, metric, min_year = 2006, max_year = 2025) {

  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value),
      value = log1p(value)
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "y"
    ) %>%
    arrange(inst_id)

  year_cols <- paste0("y", min_year:max_year)

  wide <- wide %>%
    select(inst_id, name, all_of(year_cols)) %>%
    filter(if_any(all_of(year_cols), ~ !is.na(.x)))

  mat <- wide %>%
    select(all_of(year_cols)) %>%
    as.matrix()

  rownames(mat) <- wide$inst_id

  list(
    wide = wide,
    mat = mat,
    year_cols = year_cols
  )
}

# ============================================================
# 3. Helper: estimate convergence clubs for one metric
# ============================================================

estimate_metric_clubs <- function(data, metric, min_year = 2006, max_year = 2025) {

  prep <- prepare_metric_matrix(data, metric, min_year, max_year)

  year_cols <- prep$year_cols

  # Keep data.frame, not matrix
  wide <- prep$wide %>%
    filter(if_any(all_of(year_cols), ~ !is.na(.x)))

  # Drop rows with too many missing values
  keep <- rowMeans(is.na(wide[year_cols])) <= 0.25
  wide <- wide[keep, ]

  # Interpolate missing values row by row
  wide[year_cols] <- t(apply(wide[year_cols], 1, function(x) {
    x <- as.numeric(x)
    idx <- seq_along(x)
    ok <- !is.na(x)

    if (sum(ok) == 0) return(rep(NA_real_, length(x)))
    if (sum(ok) == 1) return(rep(x[ok], length(x)))

    approx(
      x = idx[ok],
      y = x[ok],
      xout = idx,
      rule = 2
    )$y
  }))

  # Remove zero-variance rows
  keep_var <- apply(wide[year_cols], 1, sd, na.rm = TRUE) > 0
  wide <- wide[keep_var, ]

  # findClubs needs data.frame
  clubs <- findClubs(
    wide,
    dataCols = year_cols,
    unit_names = 1,
    refCol = paste0("y", max_year),
    time_trim = 1/3
  )

  membership <- imap_dfr(clubs$clubs, function(ids, club_name) {
    tibble(
      inst_id = ids,
      club = club_name
    )
  })

  membership <- wide %>%
    select(inst_id, name) %>%
    left_join(membership, by = "inst_id") %>%
    mutate(
      metric = metric,
      club = if_else(is.na(club), "Divergent", club)
    )

  list(
    metric = metric,
    data = wide,
    clubs = clubs,
    membership = membership
  )
}

estimate_metric_clubs <- function(data, metric, min_year = 2006, max_year = 2025) {

  prep <- prepare_metric_matrix(data, metric, min_year, max_year)

  mat <- prep$mat

  # Drop rows with too many missing values
  keep <- rowMeans(is.na(mat)) <= 0.25
  mat <- mat[keep, , drop = FALSE]
  wide_keep <- prep$wide[keep, ]

  # Simple interpolation for remaining missing values
  mat <- t(apply(mat, 1, function(x) {
    if (all(is.na(x))) return(x)

    idx <- seq_along(x)
    ok <- !is.na(x)

    approx(
      x = idx[ok],
      y = x[ok],
      xout = idx,
      rule = 2
    )$y
  }))

  # Remove zero-variance institutions
  keep_var <- apply(mat, 1, sd, na.rm = TRUE) > 0
  mat <- mat[keep_var, , drop = FALSE]
  wide_keep <- wide_keep[keep_var, ]

  # Convergence club estimation
  clubs <- findClubs(
    mat,
    dataCols = 1:ncol(mat),
    refCol = ncol(mat),
    time_trim = 1/3
  )

  # Extract club membership
  membership <- imap_dfr(clubs$clubs, function(ids, club_name) {
    tibble(
      inst_id = rownames(mat)[ids],
      club = club_name
    )
  })

  membership <- wide_keep %>%
    select(inst_id, name) %>%
    left_join(membership, by = "inst_id") %>%
    mutate(
      metric = metric,
      club = if_else(is.na(club), "Divergent", club)
    )

  list(
    metric = metric,
    matrix = mat,
    clubs = clubs,
    membership = membership
  )
}

# ============================================================
# 4. Run club convergence for all metrics
# ============================================================

club_results <- map(
  metrics,
  ~ estimate_metric_clubs(
    data = df_africa,
    metric = .x,
    min_year = 2006,
    max_year = 2025
  )
)

names(club_results) <- metrics

# ============================================================
# 5. Collect membership table
# ============================================================

club_membership_all <- map_dfr(
  club_results,
  ~ .x$membership
)

write.csv(
  club_membership_all,
  "club_membership_all_metrics.csv",
  row.names = FALSE
)

# ============================================================
# 6. Summary table
# ============================================================

club_summary <- club_membership_all %>%
  count(metric, club, name = "n_institutions") %>%
  arrange(metric, club)

club_summary

write.csv(
  club_summary,
  "club_summary_all_metrics.csv",
  row.names = FALSE
)

# ============================================================
# 7. Plot club size by metric
# ============================================================

ggplot(club_summary, aes(x = club, y = n_institutions, fill = club)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Convergence clubs by metric",
    x = "Club",
    y = "Number of institutions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "club_sizes_by_metric.jpeg",
  width = 11,
  height = 7,
  dpi = 500
)











library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ConvergenceClubs)

# ============================================================
# 1. Metrics to test
# ============================================================
metrics <- c(
  "n_publication",
  "n_citations",
  "international_share_any",
  "inter_african_share",
  "extra_african_share"
)

# ============================================================
# 2. Helper: prepare wide matrix for one metric
# ============================================================
prepare_metric_matrix <- function(data, metric, min_year = 2006, max_year = 2025) {

  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value),
      value = log1p(value)
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "y"
    ) %>%
    arrange(inst_id)

  year_cols <- paste0("y", min_year:max_year)

  wide <- wide %>%
    select(inst_id, name, all_of(year_cols)) %>%
    filter(if_any(all_of(year_cols), ~ !is.na(.x)))

  mat <- wide %>%
    select(all_of(year_cols)) %>%
    as.matrix()

  rownames(mat) <- wide$inst_id

  list(
    wide = wide,
    mat = mat,
    year_cols = year_cols
  )
}

# ============================================================
# 3. Helper: estimate convergence clubs for one metric
# ============================================================
estimate_metric_clubs <- function(data, metric, min_year = 2006, max_year = 2025) {

  prep <- prepare_metric_matrix(data, metric, min_year, max_year)
  mat <- prep$mat

  # Drop rows with too many missing values
  keep <- rowMeans(is.na(mat)) <= 0.25
  mat <- mat[keep, , drop = FALSE]
  wide_keep <- prep$wide[keep, ]

  # Simple interpolation for remaining missing values
  mat <- t(apply(mat, 1, function(x) {
    if (all(is.na(x))) return(x)
    idx <- seq_along(x)
    ok <- !is.na(x)
    approx(
      x = idx[ok],
      y = x[ok],
      xout = idx,
      rule = 2
    )$y
  }))
  rownames(mat) <- wide_keep$inst_id

  # Remove zero-variance institutions
  keep_var <- apply(mat, 1, sd, na.rm = TRUE) > 0
  mat <- mat[keep_var, , drop = FALSE]
  wide_keep <- wide_keep[keep_var, ]

  # findClubs() requires a data.frame, not a matrix
  mat_df <- as.data.frame(mat)

  # Convergence club estimation
  clubs <- findClubs(
    mat_df,
    dataCols = 1:ncol(mat_df),
    refCol = ncol(mat_df),
    time_trim = 1/3
  )

  # `clubs` IS the list of clubs (club1, club2, ..., divergent),
  # each element has an $id vector of row indices into mat
  membership <- imap_dfr(clubs, function(club_obj, club_name) {
    tibble(
      inst_id = rownames(mat)[club_obj$id],
      club = club_name
    )
  })

  membership <- wide_keep %>%
    select(inst_id, name) %>%
    left_join(membership, by = "inst_id") %>%
    mutate(
      metric = metric,
      club = if_else(is.na(club) | club == "divergent", "Divergent", club)
    )

  list(
    metric = metric,
    matrix = mat,
    clubs = clubs,
    membership = membership
  )
}

# ============================================================
# 4. Run club convergence for all metrics
# ============================================================
club_results <- map(
  metrics,
  ~ estimate_metric_clubs(
    data = df_africa,
    metric = .x,
    min_year = 2006,
    max_year = 2025
  )
)
names(club_results) <- metrics

# ============================================================
# 5. Collect membership table
# ============================================================
club_membership_all <- map_dfr(
  club_results,
  ~ .x$membership
)

write.csv(
  club_membership_all,
  "club_membership_all_metrics.csv",
  row.names = FALSE
)

# ============================================================
# 6. Summary table
# ============================================================
club_summary <- club_membership_all %>%
  count(metric, club, name = "n_institutions") %>%
  arrange(metric, club)

club_summary

write.csv(
  club_summary,
  "club_summary_all_metrics.csv",
  row.names = FALSE
)

summary(club_results[["n_publication"]]$clubs)


# ============================================================
# 7. Plot club size by metric
# ============================================================
ggplot(club_summary, aes(x = club, y = n_institutions, fill = club)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Convergence clubs by metric",
    x = "Club",
    y = "Number of institutions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "club_sizes_by_metric.jpeg",
  width = 11,
  height = 7,
  dpi = 500
)

names(club_results[["n_publication"]]$clubs)


prepare_metric_wide <- function(data, metric, min_year = 2006, max_year = 2025) {

  use_log <- metric %in% c("n_publication", "n_citations")

  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value),
      value = if_else(use_log, log1p(value), value)
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "y"
    ) %>%
    arrange(inst_id)

  year_cols <- paste0("y", min_year:max_year)

  wide %>%
    select(inst_id, name, all_of(year_cols)) %>%
    filter(if_all(all_of(year_cols), ~ !is.na(.x))) %>%
    filter(if_any(all_of(year_cols), ~ .x > 0))
}

estimate_metric_clubs_no_interp <- function(data, metric, min_year = 2006, max_year = 2025) {

  wide <- prepare_metric_wide(data, metric, min_year, max_year)

  year_cols <- paste0("y", min_year:max_year)

  wide <- wide %>%
    filter(apply(select(., all_of(year_cols)), 1, sd, na.rm = TRUE) > 0)

  clubs <- findClubs(
    wide,
    dataCols = which(names(wide) %in% year_cols),
    unit_names = which(names(wide) == "inst_id"),
    refCol = which(names(wide) == paste0("y", max_year)),
    time_trim = 1/3
  )

  list(
    metric = metric,
    data = wide,
    clubs = clubs
  )
}

club_pub_test <- estimate_metric_clubs_no_interp(df_africa, "n_publication")

club_pub_test$clubs
names(club_pub_test$clubs)







library(dplyr)
library(tidyr)
library(purrr)
library(ConvergenceClubs)

# ============================================================
# 1. Metrics
# ============================================================

metrics <- c(
  "n_publication",
  "n_citations",
  "international_share_any",
  "inter_african_share",
  "extra_african_share"
)

# ============================================================
# 2. Prepare wide data
# ============================================================

prepare_metric_wide <- function(data, metric, min_year = 2006, max_year = 2025) {
  
  year_cols <- paste0("y", min_year:max_year)
  
  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value)
    )
  
  if (metric %in% c("n_publication", "n_citations")) {
    wide <- wide %>%
      mutate(value = log1p(value))
  }
  
  wide <- wide %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "y"
    ) %>%
    arrange(inst_id)
  
  missing_cols <- setdiff(year_cols, names(wide))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      wide[[col]] <- NA_real_
    }
  }
  
  wide <- wide %>%
    select(inst_id, name, all_of(year_cols))
  
  wide
}

# ============================================================
# 3. Estimate clubs for one metric
# ============================================================

estimate_metric_clubs <- function(data, metric, min_year = 2006, max_year = 2025) {

  year_cols <- paste0("y", min_year:max_year)

  wide <- prepare_metric_wide(data, metric, min_year, max_year) %>%
    mutate(
      inst_id = as.character(unlist(inst_id)),
      name = as.character(unlist(name))
    ) %>%
    filter(if_all(all_of(year_cols), ~ !is.na(.x))) %>%
    rowwise() %>%
    mutate(sd_unit = sd(c_across(all_of(year_cols)), na.rm = TRUE)) %>%
    ungroup() %>%
    filter(sd_unit > 0) %>%
    select(-sd_unit) %>%
    mutate(unit_id = row_number()) %>%
    select(unit_id, inst_id, name, all_of(year_cols))

  clubs <- findClubs(
    wide,
    dataCols = which(names(wide) %in% year_cols),
    unit_names = which(names(wide) == "unit_id"),
    refCol = which(names(wide) == paste0("y", max_year)),
    time_trim = 1/3
  )

  membership <- imap_dfr(clubs, function(club_df, club_name) {
    tibble(
      unit_id = club_df$id,
      club = club_name
    )
  }) %>%
    left_join(wide %>% select(unit_id, inst_id, name), by = "unit_id") %>%
    mutate(metric = metric)

  list(
    metric = metric,
    data = wide,
    clubs = clubs,
    membership = membership
  )
}

# ============================================================
# 4. Run all metrics
# ============================================================

club_results <- map(
  metrics,
  ~ estimate_metric_clubs(
    data = df_africa,
    metric = .x,
    min_year = 2006,
    max_year = 2025
  )
)

names(club_results) <- metrics

# ============================================================
# 5. Inspect results
# ============================================================

map(club_results, "clubs")

# Example:
club_results[["n_publication"]]$clubs

# ============================================================
# 6. Collect memberships
# ============================================================






club_membership_all <- map_dfr(
  club_results,
  ~ .x$membership
)

club_summary <- club_membership_all %>%
  count(metric, club, name = "n_institutions") %>%
  arrange(metric, club)

club_summary




library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ConvergenceClubs)

# ============================================================
# 1. Metrics to test
# ============================================================
metrics <- c(
  "n_publication",
  "n_citations",
  "international_share_any",
  "inter_african_share",
  "extra_african_share"
)

# ============================================================
# 2. Helper: prepare wide matrix for one metric
# ============================================================
prepare_metric_matrix <- function(data, metric, min_year = 2006, max_year = 2025) {

  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value),
      value = log1p(value)
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = value,
      names_prefix = "y"
    ) %>%
    arrange(inst_id)

  year_cols <- paste0("y", min_year:max_year)

  wide <- wide %>%
    select(inst_id, name, all_of(year_cols)) %>%
    filter(if_any(all_of(year_cols), ~ !is.na(.x)))

  mat <- wide %>%
    select(all_of(year_cols)) %>%
    as.matrix()

  rownames(mat) <- wide$inst_id

  list(
    wide = wide,
    mat = mat,
    year_cols = year_cols
  )
}

# ============================================================
# 3. Helper: estimate convergence clubs for one metric
# ============================================================
estimate_metric_clubs <- function(data, metric, min_year = 2006, max_year = 2025) {

  prep <- prepare_metric_matrix(data, metric, min_year, max_year)
  mat <- prep$mat

  # Drop rows with too many missing values
  keep <- rowMeans(is.na(mat)) <= 0.25
  mat <- mat[keep, , drop = FALSE]
  wide_keep <- prep$wide[keep, ]

  # Simple interpolation for remaining missing values
  mat <- t(apply(mat, 1, function(x) {
    if (all(is.na(x))) return(x)
    idx <- seq_along(x)
    ok <- !is.na(x)
    approx(
      x = idx[ok],
      y = x[ok],
      xout = idx,
      rule = 2
    )$y
  }))
  rownames(mat) <- wide_keep$inst_id
  colnames(mat) <- prep$year_cols

  # Remove zero-variance institutions
  keep_var <- apply(mat, 1, sd, na.rm = TRUE) > 0
  mat <- mat[keep_var, , drop = FALSE]
  wide_keep <- wide_keep[keep_var, ]

  # findClubs() requires a data.frame, not a matrix
  mat_df <- as.data.frame(mat)

  # Convergence club estimation
  clubs <- findClubs(
    mat_df,
    dataCols = 1:ncol(mat_df),
    refCol = ncol(mat_df),
    time_trim = 1/3
  )

  # `clubs` IS the list of clubs (club1, club2, ..., divergent),
  # each element has an $id vector of row indices into mat
  membership <- imap_dfr(clubs, function(club_obj, club_name) {
    tibble(
      inst_id = rownames(mat)[club_obj$id],
      club = club_name
    )
  })

  membership <- wide_keep %>%
    select(inst_id, name) %>%
    left_join(membership, by = "inst_id") %>%
    mutate(
      metric = metric,
      club = if_else(is.na(club) | club == "divergent", "Divergent", club)
    )

  list(
    metric = metric,
    matrix = mat,
    clubs = clubs,
    membership = membership
  )
}

# ============================================================
# 4. Run club convergence for all metrics
# ============================================================
club_results <- map(
  metrics,
  ~ estimate_metric_clubs(
    data = df_africa,
    metric = .x,
    min_year = 2006,
    max_year = 2025
  )
)
names(club_results) <- metrics

# ============================================================
# 5. Collect membership table
# ============================================================
club_membership_all <- map_dfr(
  club_results,
  ~ .x$membership
)

write.csv(
  club_membership_all,
  "club_membership_all_metrics.csv",
  row.names = FALSE
)

# ============================================================
# 6. Summary table
# ============================================================
club_summary <- club_membership_all %>%
  count(metric, club, name = "n_institutions") %>%
  arrange(metric, club)

club_summary

write.csv(
  club_summary,
  "club_summary_all_metrics.csv",
  row.names = FALSE
)

# ============================================================
# 7. Plot club size by metric
# ============================================================
ggplot(club_summary, aes(x = club, y = n_institutions, fill = club)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Convergence clubs by metric",
    x = "Club",
    y = "Number of institutions"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "club_sizes_by_metric.jpeg",
  width = 11,
  height = 7,
  dpi = 500
)

# ============================================================
# 8. Extract relative transition paths (Phillips-Sul h_it) per club
# ============================================================
extract_transition_paths <- function(result) {
  h_mat <- computeH(result$matrix, quantity = "h")

  as.data.frame(h_mat) %>%
    mutate(inst_id = rownames(h_mat)) %>%
    pivot_longer(-inst_id, names_to = "year", values_to = "h") %>%
    mutate(year = as.integer(gsub("^y", "", year))) %>%
    left_join(
      result$membership %>% select(inst_id, club),
      by = "inst_id"
    ) %>%
    mutate(metric = result$metric)
}

transition_paths_all <- map_dfr(club_results, extract_transition_paths)

club_avg_paths <- transition_paths_all %>%
  group_by(metric, club, year) %>%
  summarise(mean_h = mean(h, na.rm = TRUE), .groups = "drop")

write.csv(transition_paths_all, "transition_paths_all_metrics.csv", row.names = FALSE)
write.csv(club_avg_paths, "club_avg_transition_paths.csv", row.names = FALSE)

# ============================================================
# 9. Plot club trajectories (relative transition paths) by metric
# ============================================================
ggplot(transition_paths_all, aes(x = year, y = h, group = inst_id, color = club)) +
  geom_line(alpha = 0.15, linewidth = 0.3) +
  geom_line(
    data = club_avg_paths,
    aes(x = year, y = mean_h, group = club, color = club),
    linewidth = 1.3
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Relative transition paths by convergence club",
    subtitle = "Thin lines: individual institutions \u00b7 thick lines: club average",
    x = "Year",
    y = expression(h[it]),
    color = "Club"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "club_transition_paths_by_metric.jpeg",
  width = 12,
  height = 8,
  dpi = 500
)

# ============================================================
# 10. Diagnose why a metric collapses into one big club
# ============================================================

# A) Print club summary + the whole-panel test statistic for each metric.
#    If t_all > -1.65, findClubs() stops at step 1 and returns ONE club
#    for the whole panel -- it never even tries to split it. cstar has
#    no effect on this first test, only on splitting AFTER it fails.
walk(club_results, function(result) {
  cat("\n====", result$metric, "====\n")
  print(summary(result$clubs))
  if (length(result$clubs) == 1) {
    tval <- result$clubs[[1]]$model["tvalue"]
    cat("Whole-panel t-statistic:", round(tval, 3),
        "(threshold = -1.65; algorithm stops here if t > -1.65)\n")
  }
})

# B) Plot cross-sectional variance H_t for each metric.
#    H_t -> 0 as t grows is the visual signature of convergence to one club.
#    If H_t is already nearly flat near 0 across the whole period, that's
#    why the panel collapses into a single club.
extract_Ht <- function(result) {
  Ht <- computeH(result$matrix, quantity = "H")
  tibble(
    year = as.integer(gsub("^y", "", names(Ht))),
    Ht = as.numeric(Ht),
    metric = result$metric
  )
}

Ht_all <- map_dfr(club_results, extract_Ht)

ggplot(Ht_all, aes(x = year, y = Ht)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Cross-sectional variance H_t by metric",
    subtitle = "H_t shrinking toward 0 is why the panel collapses into a single club",
    x = "Year",
    y = expression(H[t])
  ) +
  theme_minimal(base_size = 13)

ggsave("Ht_by_metric.jpeg", width = 12, height = 8, dpi = 500)

# ============================================================
# 11. Re-run on RAW (non-log) values to check if log1p is the cause
# ============================================================
prepare_metric_matrix_raw <- function(data, metric, min_year = 2006, max_year = 2025) {
  wide <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    group_by(inst_id, name, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(value = if_else(is.nan(value), NA_real_, value)) %>%  # no log1p here
    pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
    arrange(inst_id)

  year_cols <- paste0("y", min_year:max_year)
  wide <- wide %>%
    select(inst_id, name, all_of(year_cols)) %>%
    filter(if_any(all_of(year_cols), ~ !is.na(.x)))

  mat <- wide %>% select(all_of(year_cols)) %>% as.matrix()
  rownames(mat) <- wide$inst_id

  list(wide = wide, mat = mat, year_cols = year_cols)
}

# Quick check: compare whole-panel t-stat on raw vs. log1p scale
check_scale_sensitivity <- function(data, metric, min_year = 2006, max_year = 2025) {
  prep_raw <- prepare_metric_matrix_raw(data, metric, min_year, max_year)
  mat <- prep_raw$mat
  keep <- rowMeans(is.na(mat)) <= 0.25
  mat <- mat[keep, , drop = FALSE]
  mat <- t(apply(mat, 1, function(x) {
    if (all(is.na(x))) return(x)
    idx <- seq_along(x); ok <- !is.na(x)
    approx(x = idx[ok], y = x[ok], xout = idx, rule = 2)$y
  }))
  keep_var <- apply(mat, 1, sd, na.rm = TRUE) > 0
  mat <- mat[keep_var, , drop = FALSE]

  H_all <- computeH(mat)
  mod_all <- estimateMod(H_all, time_trim = 1/3, HACmethod = "FQSB")
  tibble(metric = metric, scale = "raw", tvalue = mod_all["tvalue"])
}

scale_check <- map_dfr(metrics, ~ check_scale_sensitivity(df_africa, .x))
scale_check






transition_quantiles <- transition_all %>%
  group_by(metric, year) %>%
  summarise(
    p10 = quantile(h_it, 0.10, na.rm = TRUE),
    p25 = quantile(h_it, 0.25, na.rm = TRUE),
    p50 = quantile(h_it, 0.50, na.rm = TRUE),
    p75 = quantile(h_it, 0.75, na.rm = TRUE),
    p90 = quantile(h_it, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

p_transition_quantiles <- ggplot(transition_quantiles, aes(x = year)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.30) +
  geom_line(aes(y = p50), linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(trans = "log1p") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Distribution of relative transition paths",
    subtitle = "Bands show p10–p90 and p25–p75 of h_it",
    x = "Year",
    y = expression(h[it])
  ) +
  theme_minimal(base_size = 13)

p_transition_quantiles

ggsave(
  "relative_transition_quantile_bands.jpeg",
  p_transition_quantiles,
  width = 12,
  height = 8,
  dpi = 500
)










library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# ------------------------------------------------------------
# 1. Function to compute relative transition paths h_it
# ------------------------------------------------------------

compute_transition_paths <- function(data, metric, min_year = 2006, max_year = 2025) {
  
  use_log <- metric %in% c("n_publication", "n_citations")
  
  df <- data %>%
    filter(year >= min_year, year <= max_year) %>%
    select(inst_id, name, year, value = all_of(metric)) %>%
    mutate(
      value = if_else(is.nan(value), NA_real_, value)
    )
  
  if (use_log) {
    df <- df %>%
      mutate(value = log1p(value))
  }
  
  df %>%
    group_by(year) %>%
    mutate(
      mean_value = mean(value, na.rm = TRUE),
      h_it = value / mean_value
    ) %>%
    ungroup() %>%
    filter(!is.na(h_it), is.finite(h_it))
}

# ------------------------------------------------------------
# 2. Plot relative transition paths for one metric
# ------------------------------------------------------------

plot_transition_paths <- function(data, metric, min_year = 2006, max_year = 2025) {
  
  trans <- compute_transition_paths(data, metric, min_year, max_year)
  
  # Keep only complete units for visual clarity
  complete_ids <- trans %>%
    count(inst_id) %>%
    filter(n == length(min_year:max_year)) %>%
    pull(inst_id)
  
  trans_complete <- trans %>%
    filter(inst_id %in% complete_ids)
  
  ggplot(trans_complete, aes(x = year, y = h_it, group = inst_id)) +
    geom_line(alpha = 0.12, linewidth = 0.25) +
    stat_summary(
      aes(group = 1),
      fun = median,
      geom = "line",
      linewidth = 1.2,
      color = "black"
    ) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.7
    ) +
    scale_y_continuous(
      trans = "log1p"
    ) +
    labs(
      title = paste0("Relative transition paths: ", metric),
      subtitle = "Each line is one university; black line is the yearly median",
      x = "Year",
      y = expression(h[it] == x[it] / bar(x)[t])
    ) +
    theme_minimal(base_size = 13)
}

p_pub_transition <- plot_transition_paths(df_africa, "n_publication")

p_pub_transition

ggsave(
  "relative_transition_paths_publications.jpeg",
  p_pub_transition,
  width = 10,
  height = 7,
  dpi = 500
)




metrics <- c(
  "n_publication",
  "n_citations",
  "international_share_any",
  "inter_african_share",
  "extra_african_share"
)

transition_all <- map_dfr(
  metrics,
  ~ compute_transition_paths(df_africa, .x) %>%
    mutate(metric = .x)
)

# Keep complete trajectories by metric
transition_complete <- transition_all %>%
  group_by(metric, inst_id) %>%
  filter(n() == length(2006:2025)) %>%
  ungroup()

p_transition_all <- ggplot(
  transition_complete,
  aes(x = year, y = h_it, group = inst_id)
) +
  geom_line(alpha = 0.07, linewidth = 0.2) +
  stat_summary(
    aes(group = metric),
    fun = median,
    geom = "line",
    linewidth = 1,
    color = "black"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_y_continuous(trans = "log1p") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Relative transition paths by metric",
    subtitle = "Convergence would imply paths shrinking toward h_it = 1",
    x = "Year",
    y = expression(h[it])
  ) +
  theme_minimal(base_size = 13)

p_transition_all

ggsave(
  "relative_transition_paths_all_metrics.jpeg",
  p_transition_all,
  width = 12,
  height = 8,
  dpi = 500
)



pois_trap <- fepois(
  n_publication ~
    lag(n_publication) +
    international_share_any |
    inst_id + year,
  data = df_africa) 

summary(pois_trap, diagnostics = TRUE)

library(dplyr)
library(fixest)

df_model <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    L1_n_international_any = lag(n_international_any, 1),
    L1_n_inter_african     = lag(n_inter_african, 1),
    L1_n_extra_african     = lag(n_extra_african, 1),
    L1_gdp_cap             = lag(gdp_cap, 1),
    L1_gdp_growth          = lag(gdp_growth, 1),
    L1_internet            = lag(internet, 1),
    L1_electricity         = lag(electricity, 1),
    L1_tertiary_enrol      = lag(tertiary_enrol, 1),
    L1_education_exp       = lag(education_exp, 1),
    L1_rd_exp              = lag(rd_exp, 1),
    L1_researchers         = lag(researchers_per_million, 1),

    log_L1_international_any = log1p(L1_n_international_any),
    log_L1_inter_african     = log1p(L1_n_inter_african),
    log_L1_extra_african     = log1p(L1_n_extra_african),

    country_year = interaction(country_code, year, drop = TRUE)
  ) %>%
  ungroup()

ppml_main <- fepois(
  n_publication ~
    log_L1_inter_african +
    log_L1_extra_african +
    L1_gdp_cap +
    L1_gdp_growth +
    L1_internet +
    L1_electricity +
    L1_tertiary_enrol +
    L1_education_exp |
    inst_id + year + country_year,
  data = df_model,
  cluster = ~ country_code
)

summary(ppml_main)








library(dplyr)
library(fixest)
library(slider)

# ------------------------------------------------------------
# 1. Prepare panel
# ------------------------------------------------------------

df_model <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    # Visibility
    L2_log_citations = lag(log1p(n_citations), 2),

    # Collaboration structure, lagged to reduce simultaneity
    L2_extra_share = lag(extra_african_share, 2),
    L2_inter_share = lag(inter_african_share, 2),

    # Persistent collaboration exposure: 3-year lagged moving average
    MA_extra_share = slide_dbl(
      lag(extra_african_share, 1),
      mean,
      .before = 2,
      .complete = TRUE,
      na.rm = TRUE
    ),

    MA_inter_share = slide_dbl(
      lag(inter_african_share, 1),
      mean,
      .before = 2,
      .complete = TRUE,
      na.rm = TRUE
    ),

    # University age
    age = year - founded_date,
    log_age = log1p(age)
  ) %>%
  ungroup() %>%
  mutate(
    country_year = interaction(country_code, year, drop = TRUE)
  )

# ------------------------------------------------------------
# 2. PPML models
# ------------------------------------------------------------

# Baseline ACE model
m1 <- fepois(
  n_publication ~
    ace_1 + ace_2 |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

# Add past visibility
m2 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

# Add lagged collaboration structure
m3 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_extra_share +
    L2_inter_share |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

# Add persistent collaboration exposure
m4 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    MA_extra_share +
    MA_inter_share |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

# Stronger FE: country-year absorbs national macro shocks
m5 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    MA_extra_share +
    MA_inter_share |
    inst_id + country_year,
  data = df_model,
  cluster = ~ country_code
)

# Mechanism: does ACE interact with collaboration exposure?
m6 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    MA_extra_share +
    MA_inter_share +
    ace_1:MA_extra_share +
    ace_2:MA_extra_share |
    inst_id + country_year,
  data = df_model,
  cluster = ~ country_code
)

# ------------------------------------------------------------
# 3. Global regression table
# ------------------------------------------------------------

etable(
  m1, m2, m3, m4, m5, m6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    L2_log_citations = "Lagged citations, t-2",
    L2_extra_share = "Extra-African collaboration share, t-2",
    L2_inter_share = "Inter-African collaboration share, t-2",
    MA_extra_share = "Extra-African collaboration share, lagged 3-year avg.",
    MA_inter_share = "Inter-African collaboration share, lagged 3-year avg.",
    "ace_1:MA_extra_share" = "ACE I × extra-African collaboration",
    "ace_2:MA_extra_share" = "ACE II × extra-African collaboration"
  ),
  fitstat = ~ n + ll + pr2 + bic,
  se.below = TRUE,
  tex = TRUE,
  file = "ppml_visibility_collaboration_table.tex"
)

# Also print in console
etable(
  m1, m2, m3, m4, m5, m6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    L2_log_citations = "Lagged citations, t-2",
    L2_extra_share = "Extra-African collaboration share, t-2",
    L2_inter_share = "Inter-African collaboration share, t-2",
    MA_extra_share = "Extra-African collaboration share, lagged 3-year avg.",
    MA_inter_share = "Inter-African collaboration share, lagged 3-year avg.",
    "ace_1:MA_extra_share" = "ACE I × extra-African collaboration",
    "ace_2:MA_extra_share" = "ACE II × extra-African collaboration"
  ),
  fitstat = ~ n + ll + pr2 + bic,
  se.below = TRUE
)


library(dplyr)

df_model <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(

    # Visibility
    L2_log_citations = lag(log1p(n_citations), 2),

    # Collaboration dummies
    L2_international = lag(as.integer(n_international_any > 0), 2),
    L2_inter_africa  = lag(as.integer(n_inter_african > 0), 2),
    L2_extra_africa  = lag(as.integer(n_extra_african > 0), 2),

    country_year = interaction(country_code, year, drop = TRUE)

  ) %>%
  ungroup()

library(fixest)

m1 <- fepois(
  n_publication ~
    ace_1 + ace_2 |
    inst_id + year,
  data = df_model,
  cluster = ~country_code
)

m2 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations |
    inst_id + year,
  data = df_model,
  cluster = ~country_code
)

m3 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_international |
    inst_id + year,
  data = df_model,
  cluster = ~country_code
)

m4 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + year,
  data = df_model,
  cluster = ~country_code
)

m5 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model,
  cluster = ~country_code
)



etable(
  m1, m2, m3, m4, m5,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    L2_log_citations = "Log citations (t-2)",
    L2_international = "International collaboration (t-2)",
    L2_inter_africa = "Inter-African collaboration (t-2)",
    L2_extra_africa = "Extra-African collaboration (t-2)"
  ),
  fitstat = ~n + pr2 + bic,
  tex = TRUE,
  file = "ppml_collaboration_dummy.tex"
)


df_model <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    log_age = log1p(pmax(year - founded_date, 0)),
    L2_log_citations = lag(log1p(n_citations), 2),

    cum_pub_lag = lag(cumsum(replace_na(n_publication, 0)), 1),
    log_cum_pub_lag = log1p(cum_pub_lag),

    L2_international = lag(as.integer(n_international_any > 0), 2),
    L2_inter_africa  = lag(as.integer(n_inter_african > 0), 2),
    L2_extra_africa  = lag(as.integer(n_extra_african > 0), 2)
  ) %>%
  ungroup() %>%
  mutate(country_year = interaction(country_code, year, drop = TRUE))

m_main <- fepois(
  n_publication ~
    ace_1 + ace_2 + deltas_1 +
    L2_log_citations +
    log_cum_pub_lag +
    log_age +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model,
  cluster = ~ country_code
)

summary(m_main)






library(dplyr)
library(tidyr)
library(fixest)

# ------------------------------------------------------------
# 1. Build modelling data once
# ------------------------------------------------------------

df_model <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    # University age
    age = year - founded_date,
    log_age = log1p(pmax(age, 0)),

    # Past visibility
    L2_log_citations = lag(log1p(n_citations), 2),

    # Accumulated past production
    cum_pub_lag = lag(cumsum(replace_na(n_publication, 0)), 1),
    log_cum_pub_lag = log1p(cum_pub_lag),

    # Lagged collaboration dummies
    L2_international = lag(as.integer(n_international_any > 0), 2),
    L2_inter_africa  = lag(as.integer(n_inter_african > 0), 2),
    L2_extra_africa  = lag(as.integer(n_extra_african > 0), 2)
  ) %>%
  ungroup() %>%
  mutate(
    country_year = interaction(country_code, year, drop = TRUE)
  )

# ------------------------------------------------------------
# 2. PPML specifications
# ------------------------------------------------------------

m1 <- fepois(
  n_publication ~
    ace_1 + ace_2 |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

m2 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

m3 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_international |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

m4 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + year,
  data = df_model,
  cluster = ~ country_code
)

m5 <- fepois(
  n_publication ~
    ace_1 + ace_2 +
    L2_log_citations +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model,
  cluster = ~ country_code
)

m6 <- fepois(
  n_publication ~
    ace_1 + ace_2 + deltas_1 +
    L2_log_citations +
    log_cum_pub_lag +
    log_age +
    L2_inter_africa +
    L2_extra_africa |
    inst_id + country_year,
  data = df_model,
  cluster = ~ country_code
)

# ------------------------------------------------------------
# 3. Global regression table
# ------------------------------------------------------------

etable(
  m1, m2, m3, m4, m5, m6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    deltas_1 = "DELTAS I",
    L2_log_citations = "Log citations (t-2)",
    log_cum_pub_lag = "Cumulative past publications",
    log_age = "Log university age",
    L2_international = "International collaboration dummy (t-2)",
    L2_inter_africa = "Inter-African collaboration dummy (t-2)",
    L2_extra_africa = "Extra-African collaboration dummy (t-2)"
  ),
  fitstat = ~ n + pr2 + bic,
  se.below = TRUE,
  tex = TRUE,
  file = "ppml_full_specifications.tex"
)

# Console output
etable(
  m1, m2, m3, m4, m5, m6,
  dict = c(
    ace_1 = "ACE I",
    ace_2 = "ACE II",
    deltas_1 = "DELTAS I",
    L2_log_citations = "Log citations (t-2)",
    log_cum_pub_lag = "Cumulative past publications",
    log_age = "Log university age",
    L2_international = "International collaboration dummy (t-2)",
    L2_inter_africa = "Inter-African collaboration dummy (t-2)",
    L2_extra_africa = "Extra-African collaboration dummy (t-2)"
  ),
  fitstat = ~ n + pr2 + bic,
  se.below = TRUE
)










