df_africa_rest <- df_africa %>%
  filter(
    name %in%
      pub_totals %>%
      filter(total_publications > q1) %>%
      pull(name)
  )

pub_mat <- df_africa_rest |>
  group_by(name, year) |>
  summarise(
    n_publication = sum(n_publication, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    year = paste0("y", year)
  ) |>
  pivot_wider(
    names_from = year,
    values_from = n_publication,
    values_fill = 0
  ) |>
  arrange(name)

# 2. Convert tibble to plain data.frame
pub_mat <- as.data.frame(pub_mat)

# 3. Force name column to character
pub_mat$name <- as.character(pub_mat$name)

# 4. Force all year columns to numeric
year_cols <- grep("^y", names(pub_mat))

pub_mat[year_cols] <- lapply(
  pub_mat[year_cols],
  function(x) as.numeric(as.character(x))
)

# 5. Optional but recommended: use log publications
pub_mat[year_cols] <- lapply(
  pub_mat[year_cols],
  log1p
)

# 7. Run club convergence
clubs <- findClubs(
  pub_mat,
  dataCols = year_cols,
  unit_names = 1,
  refCol = max(year_cols)
)

summary(clubs)

pub_mat_2 <- pub_mat |>

  filter(name %in% clubs$club2$unit_names)

year_cols_2 <- grep("^y", names(pub_mat_2))

clubs_2 <- findClubs(

  pub_mat_2,

  dataCols = year_cols_2,

  unit_names = 1,

  refCol = max(year_cols_2)

)

summary(clubs_2)

final_groups <- data.frame(

  name = c(final_club1, final_divergent),

  group = c(

    rep("club1_convergent", length(final_club1)),

    rep("divergent", length(final_divergent))

  )

)


# Compute mean publications per institution over the full panel
inst_means <- df_africa |>
  group_by(name) |>
  summarise(
    mean_pub = mean(n_publication, na.rm = TRUE),
    .groups = "drop"
  )

p10 <- quantile(inst_means$mean_pub, probs = 0.10, na.rm = TRUE)

inst_keep <- inst_means |>
  filter(mean_pub > p10) |>
  pull(name)

cat(sprintf(
  "Dropped %d institutions (bottom decile, threshold = %.2f mean pubs)\n",
  nrow(inst_means) - length(inst_keep),
  p10
))

pub_mat_filtered <- pub_mat |>
  filter(name %in% inst_keep)

# Recompute year columns after filtering
year_cols_filtered <- grep("^y", names(pub_mat_filtered))

# Safety checks
pub_mat_filtered <- as.data.frame(pub_mat_filtered)
pub_mat_filtered$name <- as.character(pub_mat_filtered$name)

pub_mat_filtered[year_cols_filtered] <- lapply(
  pub_mat_filtered[year_cols_filtered],
  function(x) {
    x <- as.numeric(as.character(x))
    x[!is.finite(x)] <- 0
    x
  }
)

clubs <- findClubs(
  pub_mat_filtered,
  dataCols   = year_cols_filtered,
  unit_names = 1,
  refCol     = max(year_cols_filtered)
)

summary(clubs)


inst_means <- df_africa |>

  group_by(name) |>

  summarise(

    mean_pub = mean(n_publication, na.rm = TRUE)

  )

ggplot(inst_means, aes(mean_pub)) +

  geom_histogram(bins = 50)

growth_df <- df_africa |>
  group_by(name) |>
  summarise(
    pub_initial = n_publication[year == min(year)],
    pub_final = n_publication[year == max(year)],
    growth = log1p(pub_final) - log1p(pub_initial),
    .groups = "drop"
  )

growth_df <- growth_df |>
  mutate(
    low_initial = pub_initial <= quantile(pub_initial, 0.25, na.rm = TRUE)
  )

model_threshold <- lm(
  growth ~ log1p(pub_initial) + low_initial,
  data = growth_df
)

summary(model_threshold)


library(dplyr)
library(ggplot2)

plot_df <- growth_df |>
  mutate(percentile = ntile(pub_initial, 20)) |>
  group_by(percentile) |>
  summarise(
    mean_initial = mean(pub_initial, na.rm = TRUE),
    mean_growth = mean(growth, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df,
       aes(mean_initial, mean_growth)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_log10() +
  labs(
    x = "Initial publications",
    y = "Average growth",
    title = "Growth by initial publication percentile"
  ) +
  theme_minimal()

club_growth <- ggplot(growth_df,
       aes(log1p(pub_initial), growth)) +
  geom_point(alpha = .15) +
  geom_smooth(
    method = "loess",
    span = 0.75,
    se = TRUE
  ) +
  theme_minimal()

ggsave('club_growth.jpeg', club_growth)
