df_africa <- read_csv('Data/df_africa.csv')

df_trans <- df_africa %>%
  arrange(inst_id, year) %>%
  mutate(
    log_pub = log1p(n_publication),
    log_cit = log1p(n_citations),
    citations_per_pub = if_else(
      n_publication > 0,
      n_citations / n_publication,
      0
    ),
    log_cpp = log1p(citations_per_pub)
  ) %>%
  group_by(country_code, year) %>%
  mutate(
    rel_log_pub = log_pub - mean(log_pub, na.rm = TRUE),
    rel_log_cit = log_cit - mean(log_cit, na.rm = TRUE),
    rel_log_cpp = log_cpp - mean(log_cpp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(inst_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    rel_log_pub_t5 = dplyr::lead(rel_log_pub, 5),
    rel_log_cit_t5 = dplyr::lead(rel_log_cit, 5),
    rel_log_cpp_t5 = dplyr::lead(rel_log_cpp, 5),
    year_t5 = dplyr::lead(year, 5)
  ) %>%
  ungroup() %>%
  filter(year_t5 == year + 5) %>%
  select(
    inst_id,
    name,
    country_code,
    year,
    rel_log_pub_t = rel_log_pub,
    rel_log_pub_t5,
    rel_log_cit_t = rel_log_cit,
    rel_log_cit_t5,
    rel_log_cpp_t = rel_log_cpp,
    rel_log_cpp_t5
  )

transition_path <- function(data, xvar, yvar, title, xlab, ylab,
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

  fit <- loess(y ~ x, data = df, span = span, degree = 1)
  grid$fit <- predict(fit, newdata = grid)
  ids      <- unique(df$name)
  df_split <- split(df, df$name) 
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

plan(multisession, workers = 2)

# The process of kernel estimation can be very long (~30mins, you can lower the B = 'num' to speed up, but lose precision)

set.seed(123)
country_year_plots <- (
  transition_path(
    df_trans,
    "rel_log_pub_t",
    "rel_log_pub_t5",
    "Publications",
    "Position at t",
    "Position at t + 5",
    B = 500
  ) |
    transition_path(
      df_trans,
      "rel_log_cit_t",
      "rel_log_cit_t5",
      "Citations",
      "Position at t",
      "Position at t + 5",
      B = 500
    ) |
    transition_path(
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

plan(sequential) 

ggsave('Output/country_plots_transition.jpeg', country_year_plots, width = 13, height = 6)

gam_fit <- gam(
  rel_log_cpp_t5 ~ s(rel_log_cpp_t),
  data = df_trans
)

jpeg(
  "Output/gam_transition_plot.jpeg",
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
