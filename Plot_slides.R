library(dplyr)
library(countrycode)

library(dplyr)
library(countrycode)

tw_panel <- tw_panel |>
  mutate(

    continent = countrycode(
      location,
      origin = "country.name",
      destination = "continent"
    ),

    Africa = as.integer(continent == "Africa"),

    Europe = as.integer(continent == "Europe"),

    Asia = as.integer(continent == "Asia"),

    Oceania = as.integer(continent == "Oceania"),

    US = as.integer(location == "United States"),

    LATAM = as.integer(
      continent == "Americas" &
      !location %in% c("United States", "Canada")
    )
  )

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

year_plot <- 2025

region_share <- tw_panel |>
  filter(Year == year_plot) |>
  summarise(
    Africa = sum(Africa, na.rm = TRUE),
    Europe = sum(Europe, na.rm = TRUE),
    US = sum(US, na.rm = TRUE),
    LATAM = sum(LATAM, na.rm = TRUE),
    Asia = sum(Asia, na.rm = TRUE),
    Oceania = sum(Oceania, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "region",
    values_to = "n"
  ) |>
  mutate(
    share = n / sum(n),
    label = paste0(region, "\n", percent(share, accuracy = 0.1))
  )

pie_region <- ggplot(region_share, aes(
  x = "",
  y = share,
  fill = region
)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = paste("Regional composition of the ranking in", year_plot),
    fill = "Region"
  ) +
  theme_void()

pie_region

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

year_plot <- 2025

region_share <- tw_panel |>
  filter(Year == year_plot) |>
  summarise(
    Africa = sum(Africa, na.rm = TRUE),
    Europe = sum(Europe, na.rm = TRUE),
    US = sum(US, na.rm = TRUE),
    LATAM = sum(LATAM, na.rm = TRUE),
    Asia = sum(Asia, na.rm = TRUE),
    Oceania = sum(Oceania, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "region",
    values_to = "n"
  ) |>
  mutate(
    share = n / sum(n),
    region = fct_reorder(region, share)
  )

p_region <- ggplot(region_share, aes(
  x = region,
  y = share,
  fill = region
)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    hjust = -0.1,
    size = 5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent,
    limits = c(0, max(region_share$share) * 1.15)
  ) +
  labs(
    title = paste("Regional composition of the ranking in", year_plot),
    x = NULL,
    y = "Share of ranked universities"
  ) +
  theme_minimal(base_size = 14)

p_region

ggsave(
  paste0("region_share_ranking_", year_plot, ".jpeg"),
  p_region,
  width = 9,
  height = 6,
  dpi = 300
)

rank_region <- tw_panel |>

  mutate(

    region = case_when(

      Africa == 1 ~ "Africa",

      Europe == 1 ~ "Europe",

      US == 1 ~ "US",

      LATAM == 1 ~ "LATAM",

      Asia == 1 ~ "Asia",

      Oceania == 1 ~ "Oceania"

    )

  ) |>

  group_by(Year, region) |>

  summarise(

    mean_rank = mean(rank, na.rm = TRUE),

    n_univ = n(),

    .groups = "drop"

  )

rank_region <- ggplot(

  rank_region,

  aes(

    x = Year,

    y = mean_rank,

    color = region

  )

) +

  geom_line(linewidth = 1) +

  geom_point(size = 2) +

  scale_y_reverse() +

  labs(

    title = "Mean university rank by region over time",

    x = "Year",

    y = "Mean rank",

    color = "Region"

  ) +

  theme_minimal(base_size = 14)


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

rank_region_pct <- tw_panel |>
  group_by(Year) |>
  mutate(
    n_ranked = max(rank, na.rm = TRUE),
    percentile_rank = 1 - (rank - 1) / (n_ranked - 1)
  ) |>
  ungroup() |>
  pivot_longer(
    cols = c(Africa, Europe, US, LATAM, Asia, Oceania),
    names_to = "region",
    values_to = "dummy"
  ) |>
  filter(dummy == 1) |>
  group_by(Year, region) |>
  summarise(
    mean_percentile_rank = mean(percentile_rank, na.rm = TRUE),
    .groups = "drop"
  )

p_rank_pct <- ggplot(
  rank_region_pct,
  aes(
    x = Year,
    y = mean_percentile_rank,
    color = region
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Mean percentile rank \n by region over time",
    x = "Year",
    y = "Mean percentile rank",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

p_rank_pct
ggsave('perc_region.jpeg', dpi = 300)


ggsave(
  "perc_region.jpeg",
  p_rank_pct,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "region_share_ranking_2025.jpeg",
  p_region,
  width = 8,
  height = 5,
  dpi = 300
)
