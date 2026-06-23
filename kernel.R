library(dplyr)
library(ggplot2)
library(np)
library(patchwork)

# --------------------------------------------------
# 1. Build transition panel
#    Raw variables + net-of-country-year variables
# --------------------------------------------------

df_trans <- df_africa %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    cit_per_pub = ifelse(
      n_publication > 0,
      n_citations / n_publication,
      NA_real_
    ),
    log_cpp = log1p(cit_per_pub)
  ) %>%
  group_by(country_code, year) %>%
  mutate(
    rel_log_pub = log_pub - mean(log_pub, na.rm = TRUE),
    rel_log_cit = log_cit - mean(log_cit, na.rm = TRUE),
    rel_log_cpp = log_cpp - mean(log_cpp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(
    log_pub_t = log_pub,
    log_pub_t5 = dplyr::lead(log_pub, 5),
    rel_log_pub_t = rel_log_pub,
    rel_log_pub_t5 = dplyr::lead(rel_log_pub, 5),

    log_cit_t = log_cit,
    log_cit_t5 = dplyr::lead(log_cit, 5),
    rel_log_cit_t = rel_log_cit,
    rel_log_cit_t5 = dplyr::lead(rel_log_cit, 5),

    log_cpp_t = log_cpp,
    log_cpp_t5 = dplyr::lead(log_cpp, 5),
    rel_log_cpp_t = rel_log_cpp,
    rel_log_cpp_t5 = dplyr::lead(rel_log_cpp, 5),

    year_t5 = dplyr::lead(year, 5)
  ) %>%
  ungroup() %>%
  filter(year_t5 == year + 5)

# --------------------------------------------------
# 2. Helper: transition function
# --------------------------------------------------

make_transition <- function(data, xvar, yvar, title, xlab, ylab) {

  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )

  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )

  grid <- data.frame(
    x = seq(
      min(df[[xvar]], na.rm = TRUE),
      max(df[[xvar]], na.rm = TRUE),
      length.out = 300
    )
  )

  names(grid) <- xvar

  grid$y_hat <- predict(m, newdata = grid)

  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_line(linewidth = 1.1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 12)
}

# --------------------------------------------------
# 3. Helper: mobility function
# --------------------------------------------------

make_growth <- function(data, xvar, yvar, title, xlab, ylab) {

  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )

  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )

  grid <- data.frame(
    x = seq(
      min(df[[xvar]], na.rm = TRUE),
      max(df[[xvar]], na.rm = TRUE),
      length.out = 300
    )
  )

  names(grid) <- xvar

  grid$y_hat <- predict(m, newdata = grid)

  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_line(linewidth = 1.1) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 12)
}

# --------------------------------------------------
# 4. Raw transition functions
# --------------------------------------------------

raw_plots <- (
  make_transition(
    df_trans,
    "log_pub_t",
    "log_pub_t5",
    "Publications",
    "log(1 + publications) at t",
    "log(1 + publications) at t + 5"
  ) /
  make_transition(
    df_trans,
    "log_cit_t",
    "log_cit_t5",
    "Citations",
    "log(1 + citations) at t",
    "log(1 + citations) at t + 5"
  ) /
  make_transition(
    df_trans,
    "log_cpp_t",
    "log_cpp_t5",
    "Citations per publication",
    "log(1 + citations/publication) at t",
    "log(1 + citations/publication) at t + 5"
  )
) +
  plot_annotation(title = "Raw transition functions")

raw_plots

ggsave(
  "transition_functions_raw.jpeg",
  raw_plots,
  width = 9,
  height = 13,
  dpi = 300
)

# --------------------------------------------------
# 5. Net-of-country-year transition functions
# --------------------------------------------------

country_year_plots <- (
  make_transition(
    df_trans,
    "rel_log_pub_t",
    "rel_log_pub_t5",
    "Publications, net of country-year mean",
    "Position at t within country-year",
    "Position at t + 5 within country-year"
  ) |
  make_transition(
    df_trans,
    "rel_log_cit_t",
    "rel_log_cit_t5",
    "Citations, net of country-year mean",
    "Position at t within country-year",
    "Position at t + 5 within country-year"
  ) |
  make_transition(
    df_trans,
    "rel_log_cpp_t",
    "rel_log_cpp_t5",
    "Citations/publication, net of country-year mean",
    "Position at t within country-year",
    "Position at t + 5 within country-year"
  )
) +
  plot_annotation(title = "Transition functions net of country-year mean")

country_year_plots

ggsave(
  "transition_functions_net_country_year.jpeg",
  country_year_plots,
  width = 9,
  height = 13,
  dpi = 300
)

# --------------------------------------------------
# 6. Mobility functions
# --------------------------------------------------

df_trans <- df_trans %>%
  mutate(
    d_rel_log_pub = rel_log_pub_t5 - rel_log_pub_t,
    d_rel_log_cit = rel_log_cit_t5 - rel_log_cit_t,
    d_rel_log_cpp = rel_log_cpp_t5 - rel_log_cpp_t
  )

mobility_plots <- (
  make_growth(
    df_trans,
    "rel_log_pub_t",
    "d_rel_log_pub",
    "Publication mobility",
    "Publication position at t within country-year",
    "Change in within-country position, t to t + 5"
  ) /
  make_growth(
    df_trans,
    "rel_log_cit_t",
    "d_rel_log_cit",
    "Citation mobility",
    "Citation position at t within country-year",
    "Change in within-country position, t to t + 5"
  ) /
  make_growth(
    df_trans,
    "rel_log_cpp_t",
    "d_rel_log_cpp",
    "Citation-intensity mobility",
    "Citation-intensity position at t within country-year",
    "Change in within-country position, t to t + 5"
  )
) +
  plot_annotation(title = "Five-year mobility functions net of country-year mean")

mobility_plots

ggsave(
  "mobility_functions_net_country_year.jpeg",
  mobility_plots,
  width = 9,
  height = 13,
  dpi = 300
)

p_pub <- make_transition(
  df_trans,
  "rel_log_pub_t",
  "rel_log_pub_t5",
  "Publications",
  "Position at t within country-year",
  "Position at t+5 within country-year"
)

p_cit <- make_transition(
  df_trans,
  "rel_log_cit_t",
  "rel_log_cit_t5",
  "Citations",
  "Position at t within country-year",
  "Position at t+5 within country-year"
)

p_cpp <- make_transition(
  df_trans,
  "rel_log_cpp_t",
  "rel_log_cpp_t5",
  "Citations per publication",
  "Position at t within country-year",
  "Position at t+5 within country-year"
)

ggplot(df_africa, aes(x = n_publication)) +
  geom_histogram(bins = 100) +
  theme_minimal()











library(dplyr)
library(ggplot2)
library(np)
library(patchwork)

# --------------------------------------------------
# 1. Build robust rank-based panel
# --------------------------------------------------

df_rank <- df_africa %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    cit_per_pub = ifelse(
      n_publication > 0,
      n_citations / n_publication,
      NA_real_
    ),
    log_cpp = log1p(cit_per_pub)
  ) %>%
  group_by(country_code, year) %>%
  mutate(
    r_pub = percent_rank(log_pub),
    r_cit = percent_rank(log_cit),
    r_cpp = percent_rank(log_cpp)
  ) %>%
  ungroup()

# --------------------------------------------------
# 2. Keep institutions observed at t and t+5
# --------------------------------------------------

df_trans_rank <- df_rank %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(
    r_pub_t = r_pub,
    r_pub_t5 = dplyr::lead(r_pub, 5),

    r_cit_t = r_cit,
    r_cit_t5 = dplyr::lead(r_cit, 5),

    r_cpp_t = r_cpp,
    r_cpp_t5 = dplyr::lead(r_cpp, 5),

    year_t5 = dplyr::lead(year, 5)
  ) %>%
  ungroup() %>%
  filter(year_t5 == year + 5)

# --------------------------------------------------
# 3. Helper: rank transition
# --------------------------------------------------

make_rank_transition <- function(data, xvar, yvar, title) {

  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )

  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )

  grid <- data.frame(
    x = seq(0, 1, length.out = 300)
  )

  names(grid) <- xvar

  grid$y_hat <- predict(m, newdata = grid)

  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_line(linewidth = 1.1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      title = title,
      x = "Percentile within country-year at t",
      y = "Expected percentile within country-year at t + 5"
    ) +
    theme_minimal(base_size = 13)
}

# --------------------------------------------------
# 4. Transition plots
# --------------------------------------------------

p_rank_pub <- make_rank_transition(
  df_trans_rank,
  "r_pub_t",
  "r_pub_t5",
  "Publication-rank transition"
)

p_rank_cit <- make_rank_transition(
  df_trans_rank,
  "r_cit_t",
  "r_cit_t5",
  "Citation-rank transition"
)

p_rank_cpp <- make_rank_transition(
  df_trans_rank,
  "r_cpp_t",
  "r_cpp_t5",
  "Citation-intensity-rank transition"
)

p_rank_pub
p_rank_cit
p_rank_cpp

ggsave("rank_transition_publications.jpeg", p_rank_pub, width = 8, height = 6, dpi = 300)
ggsave("rank_transition_citations.jpeg", p_rank_cit, width = 8, height = 6, dpi = 300)
ggsave("rank_transition_citation_intensity.jpeg", p_rank_cpp, width = 8, height = 6, dpi = 300)

# --------------------------------------------------
# 5. Mobility functions
# --------------------------------------------------

df_trans_rank <- df_trans_rank %>%
  mutate(
    d_r_pub = r_pub_t5 - r_pub_t,
    d_r_cit = r_cit_t5 - r_cit_t,
    d_r_cpp = r_cpp_t5 - r_cpp_t
  )

make_rank_mobility <- function(data, xvar, yvar, title) {

  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )

  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )

  grid <- data.frame(
    x = seq(0, 1, length.out = 300)
  )

  names(grid) <- xvar

  grid$y_hat <- predict(m, newdata = grid)

  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 1.1) +
    labs(
      title = title,
      x = "Percentile within country-year at t",
      y = "Change in percentile, t to t + 5"
    ) +
    theme_minimal(base_size = 13)
}

p_mob_pub <- make_rank_mobility(
  df_trans_rank,
  "r_pub_t",
  "d_r_pub",
  "Publication-rank mobility"
)

p_mob_cit <- make_rank_mobility(
  df_trans_rank,
  "r_cit_t",
  "d_r_cit",
  "Citation-rank mobility"
)

p_mob_cpp <- make_rank_mobility(
  df_trans_rank,
  "r_cpp_t",
  "d_r_cpp",
  "Citation-intensity-rank mobility"
)

p_mob_pub
p_mob_cit
p_mob_cpp

ggsave("rank_mobility_publications.jpeg", p_mob_pub, width = 8, height = 6, dpi = 300)
ggsave("rank_mobility_citations.jpeg", p_mob_cit, width = 8, height = 6, dpi = 300)
ggsave("rank_mobility_citation_intensity.jpeg", p_mob_cpp, width = 8, height = 6, dpi = 300)



df_africa %>%
  distinct(country_code, name) %>%
  count(country_code) %>%
  summary()




library(dplyr)
library(ggplot2)
library(np)
library(patchwork)
library(purrr)

# --------------------------------------------------
# 1. Build z-score transition panel
# --------------------------------------------------

df_trans_z <- df_africa %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    cit_per_pub = ifelse(
      n_publication > 0,
      n_citations / n_publication,
      NA_real_
    ),
    log_cpp = log1p(cit_per_pub)
  ) %>%
  group_by(country_code, year) %>%
  mutate(
    n_country_year = n(),
    
    z_pub = ifelse(
      sd(log_pub, na.rm = TRUE) > 0,
      (log_pub - mean(log_pub, na.rm = TRUE)) / sd(log_pub, na.rm = TRUE),
      NA_real_
    ),
    
    z_cit = ifelse(
      sd(log_cit, na.rm = TRUE) > 0,
      (log_cit - mean(log_cit, na.rm = TRUE)) / sd(log_cit, na.rm = TRUE),
      NA_real_
    ),
    
    z_cpp = ifelse(
      sd(log_cpp, na.rm = TRUE) > 0,
      (log_cpp - mean(log_cpp, na.rm = TRUE)) / sd(log_cpp, na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(
    n_country_year >= 5
  ) %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(
    z_pub_t  = z_pub,
    z_pub_t5 = dplyr::lead(z_pub, 5),
    
    z_cit_t  = z_cit,
    z_cit_t5 = dplyr::lead(z_cit, 5),
    
    z_cpp_t  = z_cpp,
    z_cpp_t5 = dplyr::lead(z_cpp, 5),
    
    year_t5 = dplyr::lead(year, 5)
  ) %>%
  ungroup() %>%
  filter(
    year_t5 == year + 5
  ) %>%
  mutate(
    d_z_pub = z_pub_t5 - z_pub_t,
    d_z_cit = z_cit_t5 - z_cit_t,
    d_z_cpp = z_cpp_t5 - z_cpp_t
  )

# --------------------------------------------------
# 2. Helper: transition function
# --------------------------------------------------

make_transition <- function(data, xvar, yvar, title, xlab, ylab) {
  
  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )
  
  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )
  
  grid <- data.frame(
    x = seq(
      quantile(df[[xvar]], 0.01, na.rm = TRUE),
      quantile(df[[xvar]], 0.99, na.rm = TRUE),
      length.out = 300
    )
  )
  
  names(grid) <- xvar
  
  grid$y_hat <- predict(
    m,
    newdata = grid
  )
  
  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_line(linewidth = 1.1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 13)
}

# --------------------------------------------------
# 3. Helper: mobility function
# --------------------------------------------------

make_mobility <- function(data, xvar, yvar, title, xlab, ylab) {
  
  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )
  
  m <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )
  
  grid <- data.frame(
    x = seq(
      quantile(df[[xvar]], 0.01, na.rm = TRUE),
      quantile(df[[xvar]], 0.99, na.rm = TRUE),
      length.out = 300
    )
  )
  
  names(grid) <- xvar
  
  grid$y_hat <- predict(
    m,
    newdata = grid
  )
  
  ggplot(grid, aes(x = .data[[xvar]], y = y_hat)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_line(linewidth = 1.1) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 13)
}

# --------------------------------------------------
# 4. Transition plots
# --------------------------------------------------

p_z_pub <- make_transition(
  df_trans_z,
  "z_pub_t",
  "z_pub_t5",
  "Publication transition",
  "Publication position at t within country-year",
  "Expected publication position at t + 5"
)

p_z_cit <- make_transition(
  df_trans_z,
  "z_cit_t",
  "z_cit_t5",
  "Citation transition",
  "Citation position at t within country-year",
  "Expected citation position at t + 5"
)

p_z_cpp <- make_transition(
  df_trans_z,
  "z_cpp_t",
  "z_cpp_t5",
  "Citation-intensity transition",
  "Citation-intensity position at t within country-year",
  "Expected citation-intensity position at t + 5"
)

p_z_pub
p_z_cit
p_z_cpp

ggsave("z_transition_publications.jpeg", p_z_pub, width = 8, height = 6, dpi = 300)
ggsave("z_transition_citations.jpeg", p_z_cit, width = 8, height = 6, dpi = 300)
ggsave("z_transition_citation_intensity.jpeg", p_z_cpp, width = 8, height = 6, dpi = 300)

# --------------------------------------------------
# 5. Mobility plots
# --------------------------------------------------

p_mob_pub <- make_mobility(
  df_trans_z,
  "z_pub_t",
  "d_z_pub",
  "Publication mobility",
  "Publication position at t within country-year",
  "Change in publication position, t to t + 5"
)

p_mob_cit <- make_mobility(
  df_trans_z,
  "z_cit_t",
  "d_z_cit",
  "Citation mobility",
  "Citation position at t within country-year",
  "Change in citation position, t to t + 5"
)

p_mob_cpp <- make_mobility(
  df_trans_z,
  "z_cpp_t",
  "d_z_cpp",
  "Citation-intensity mobility",
  "Citation-intensity position at t within country-year",
  "Change in citation-intensity position, t to t + 5"
)

p_mob_pub
p_mob_cit
p_mob_cpp

ggsave("z_mobility_publications.jpeg", p_mob_pub, width = 8, height = 6, dpi = 300)
ggsave("z_mobility_citations.jpeg", p_mob_cit, width = 8, height = 6, dpi = 300)
ggsave("z_mobility_citation_intensity.jpeg", p_mob_cpp, width = 8, height = 6, dpi = 300)

# --------------------------------------------------
# 6. Combined figures
# --------------------------------------------------

transition_plots <- p_z_pub / p_z_cit / p_z_cpp +
  plot_annotation(
    title = "Five-year transition functions, z-scores within country-year"
  )

mobility_plots <- p_mob_pub / p_mob_cit / p_mob_cpp +
  plot_annotation(
    title = "Five-year mobility functions, z-scores within country-year"
  )

transition_plots
mobility_plots

ggsave("z_transition_all.jpeg", transition_plots, width = 9, height = 13, dpi = 300)
ggsave("z_mobility_all.jpeg", mobility_plots, width = 9, height = 13, dpi = 300)









country_year_plots +
  plot_layout(guides = "collect") &
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )

ggsave(
  "transition_functions_net_country_year.jpeg",
  country_year_plots,
  width = 18,
  height = 6,
  dpi = 300
)

ggsave(
  "transition_functions_net_country_year.jpeg",
  country_year_plots,
  width = 24,
  height = 7,
  dpi = 300
)






library(dplyr)
library(ggplot2)
library(np)
library(patchwork)

make_transition_ci <- function(data, xvar, yvar, title, xlab, ylab, B = 300) {
  
  df <- data %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[yvar]])
    )
  
  grid <- data.frame(
    x = seq(
      quantile(df[[xvar]], 0.01, na.rm = TRUE),
      quantile(df[[xvar]], 0.99, na.rm = TRUE),
      length.out = 250
    )
  )
  
  names(grid) <- xvar
  
  fit <- npreg(
    as.formula(paste(yvar, "~", xvar)),
    data = df,
    regtype = "ll"
  )
  
  grid$fit <- predict(fit, newdata = grid)
  
  boot_mat <- replicate(B, {
    idx <- sample(seq_len(nrow(df)), replace = TRUE)
    boot_df <- df[idx, ]
    
    boot_fit <- tryCatch(
      npreg(
        as.formula(paste(yvar, "~", xvar)),
        data = boot_df,
        regtype = "ll"
      ),
      error = function(e) NULL
    )
    
    if (is.null(boot_fit)) {
      rep(NA_real_, nrow(grid))
    } else {
      predict(boot_fit, newdata = grid)
    }
  })
  
  grid$lower <- apply(boot_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  grid$upper <- apply(boot_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  
  ggplot(grid, aes(x = .data[[xvar]], y = fit)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    geom_line(linewidth = 1.1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 12)
}

set.seed(123)

country_year_plots <- (
  make_transition_ci(
    df_trans,
    "rel_log_pub_t",
    "rel_log_pub_t5",
    "Publications",
    "Position at t",
    "Position at t + 5",
    B = 20
  ) |
  make_transition_ci(
    df_trans,
    "rel_log_cit_t",
    "rel_log_cit_t5",
    "Citations",
    "Position at t",
    "Position at t + 5",
    B = 20
  ) |
  make_transition_ci(
    df_trans,
    "rel_log_cpp_t",
    "rel_log_cpp_t5",
    "Citations/publication",
    "Position at t",
    "Position at t + 5",
    B = 20
  )
) +
  plot_annotation(
    title = "Transition functions net of country-year mean"
  )

country_year_plots

ggsave(
  "transition_functions_net_country_year_ci.jpeg",
  country_year_plots,
  width = 20,
  height = 6,
  dpi = 300
)
