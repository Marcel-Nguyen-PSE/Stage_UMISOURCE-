african_codes <- c(
  "DZ", "AO", "BJ", "BW", "BF", "BI", "CV", "CM", "CF", "TD",
  "KM", "CG", "CD", "CI", "DJ", "EG", "GQ", "ER", "SZ", "ET",
  "GA", "GM", "GH", "GN", "GW", "KE", "LS", "LR", "LY", "MG",
  "MW", "ML", "MR", "MU", "MA", "MZ", "NA", "NE", "NG", "RW",
  "ST", "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "TZ", "TG",
  "TN", "UG", "ZM", "ZW"
)

# ------------------------------------------------------------
# 2. Keep Africa -> non-Africa collaboration flows
# ------------------------------------------------------------

extra_africa_all <- country_flows_2025 %>%
  filter(
    origin %in% african_codes,
    !(destination %in% african_codes),
    origin != destination,
    weight > 0
  )

# ------------------------------------------------------------
# 3. Identify top African origin countries
# ------------------------------------------------------------

n_top_origins <- 5

top_origin_codes <- extra_africa_all %>%
  group_by(origin) %>%
  summarise(total_collab = sum(weight), .groups = "drop") %>%
  arrange(desc(total_collab)) %>%
  slice_head(n = n_top_origins) %>%
  pull(origin)

# ------------------------------------------------------------
# 4. Classify flows: top African origins vs rest
# ------------------------------------------------------------

extra_africa_all <- extra_africa_all %>%
  mutate(
    origin_group = if_else(
      origin %in% top_origin_codes,
      "Top African origins",
      "Other African origins"
    )
  )

# Optional: keep all flows or filter tiny ones
# Use min_weight <- 1 for exhaustive plotting
min_weight <- 1

extra_africa_all <- extra_africa_all %>%
  filter(weight >= min_weight)

# ------------------------------------------------------------
# 5. World map and country centroids
# ------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

centroids <- world %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(
    country_code = iso_a2,
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(country_code, country_name = name, lon, lat)

# ------------------------------------------------------------
# 6. Add coordinates
# ------------------------------------------------------------

extra_africa_map <- extra_africa_all %>%
  left_join(
    centroids %>%
      rename(
        origin_name = country_name,
        lon_origin = lon,
        lat_origin = lat
      ),
    by = c("origin" = "country_code")
  ) %>%
  left_join(
    centroids %>%
      rename(
        destination_name = country_name,
        lon_dest = lon,
        lat_dest = lat
      ),
    by = c("destination" = "country_code")
  ) %>%
  filter(
    !is.na(lon_origin),
    !is.na(lat_origin),
    !is.na(lon_dest),
    !is.na(lat_dest)
  )

# ------------------------------------------------------------
# 7. Separate top origins and rest
# ------------------------------------------------------------

flows_rest <- extra_africa_map %>%
  filter(origin_group == "Other African origins") %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.15, 0.75)))

flows_top <- extra_africa_map %>%
  filter(origin_group == "Top African origins") %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.25, 1)))

# ------------------------------------------------------------
# 8. Plot Africa -> non-Africa collaboration flows
# ------------------------------------------------------------

p_extra_africa <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey97",
    color = "grey78",
    linewidth = 0.20
  ) +
  
  # Other African origins: blue
  geom_curve(
    data = flows_rest,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight_scaled
    ),
    color = "#3182BD",
    curvature = 0.25,
    arrow = arrow(length = unit(0.07, "inches"), type = "closed")
  ) +
  
  # Top African origins: red
  geom_curve(
    data = flows_top,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight_scaled
    ),
    color = "#CB181D",
    curvature = 0.25,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed")
  ) +
  
  scale_linewidth_continuous(
    range = c(0.10, 3.2),
    name = "Collaborations"
  ) +
  scale_alpha_continuous(
    range = c(0.10, 0.85),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 80),
    expand = FALSE
  ) +
  labs(
    title = "Extra-African scientific collaboration flows, 2025",
    subtitle = paste0(
      "Africa to non-African countries; red arrows originate from the top ",
      n_top_origins,
      " African collaboration hubs"
    ),
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold")
  )

p_extra_africa

ggsave(
  "extra_african_collaboration_flows_2025.jpeg",
  p_extra_africa,
  width = 14,
  height = 8,
  dpi = 500
)

flows_top <- extra_africa_map %>%
  filter(origin %in% top_origin_codes)

flows_rest <- extra_africa_map %>%
  filter(!(origin %in% top_origin_codes))

plot_collaboration_map <- function(flows,
                                   line_color_low,
                                   line_color_high,
                                   title,
                                   filename){

  p <- ggplot() +

    geom_sf(
      data = world,
      fill = "grey97",
      color = "grey78",
      linewidth = 0.2
    ) +

    geom_curve(
      data = flows,
      aes(
        x = lon_origin,
        y = lat_origin,
        xend = lon_dest,
        yend = lat_dest,
        linewidth = weight,
        colour = weight
      ),
      curvature = 0.25,
      alpha = 0.9,
      arrow = arrow(
        length = unit(0.08, "inches"),
        type = "closed"
      )
    ) +

    scale_colour_gradient(
      low = line_color_low,
      high = line_color_high,
      name = "Collaborations"
    ) +

    scale_linewidth_continuous(
      range = c(0.15,3),
      guide = "none"
    ) +

    coord_sf(
      xlim = c(-180,180),
      ylim = c(-60,80),
      expand = FALSE
    ) +

    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +

    theme_void(base_size = 13) +

    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  print(p)

  ggsave(
    filename,
    p,
    width = 14,
    height = 8,
    dpi = 500
  )
}

# -------------------------------------------------------
# Top African collaboration hubs
# -------------------------------------------------------

plot_collaboration_map(
  flows = flows_top,
  line_color_low = "#FEE5D9",
  line_color_high = "#A50F15",
  title = "Extra-African collaboration flows from the major African scientific hubs (2025)",
  filename = "extra_africa_top_hubs.jpeg"
)

# -------------------------------------------------------
# Remaining African countries
# -------------------------------------------------------

plot_collaboration_map(
  flows = flows_rest,
  line_color_low = "#DEEBF7",
  line_color_high = "#08519C",
  title = "Extra-African collaboration flows from the remaining African countries (2025)",
  filename = "extra_africa_other_countries.jpeg"
)



# 1. Keep only strongest links
flows_top_clean <- flows_top %>%
  group_by(origin) %>%
  slice_max(weight, n = 8, with_ties = FALSE) %>%
  ungroup()

flows_rest_clean <- flows_rest %>%
  group_by(origin) %>%
  slice_max(weight, n = 3, with_ties = FALSE) %>%
  ungroup()

plot_collaboration_map <- function(flows,
                                   line_color_low,
                                   line_color_high,
                                   title,
                                   filename){

  flows <- flows %>%
    mutate(weight_plot = log1p(weight))

  p <- ggplot() +
    geom_sf(
      data = world,
      fill = "grey98",
      color = "grey85",
      linewidth = 0.15
    ) +
    geom_curve(
      data = flows,
      aes(
        x = lon_origin,
        y = lat_origin,
        xend = lon_dest,
        yend = lat_dest,
        linewidth = weight_plot,
        colour = weight
      ),
      curvature = 0.18,
      alpha = 0.75,
      arrow = arrow(length = unit(0.06, "inches"), type = "closed")
    ) +
    scale_colour_gradient(
      low = line_color_low,
      high = line_color_high,
      name = "Collaborations"
    ) +
    scale_linewidth_continuous(
      range = c(0.15, 2.4),
      guide = "none"
    ) +
    coord_sf(
      xlim = c(-120, 160),
      ylim = c(-45, 70),
      expand = FALSE
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggsave(filename, p, width = 13, height = 7.5, dpi = 500)
  p
}

plot_collaboration_map(
  flows_top_clean,
  "#FEE5D9",
  "#A50F15",
  "Extra-African collaboration flows from major African hubs, 2025",
  "extra_africa_top_hubs_clean.jpeg"
)

plot_collaboration_map(
  flows_rest_clean,
  "#DEEBF7",
  "#08519C",
  "Extra-African collaboration flows from other African countries, 2025",
  "extra_africa_other_countries_clean.jpeg"
)






# ------------------------------------------------------------
# 3. Define African origin groups
# ------------------------------------------------------------

origin_groups <- tibble::tribble(
  ~origin, ~origin_group,

  # Maghreb
  "DZ", "Maghreb",
  "MA", "Maghreb",
  "TN", "Maghreb",
  "LY", "Maghreb",
  "EG", "Maghreb",

  # South Africa as country
  "ZA", "South Africa",

  # Central Africa
  "CM", "Central Africa",
  "CF", "Central Africa",
  "TD", "Central Africa",
  "CG", "Central Africa",
  "CD", "Central Africa",
  "GQ", "Central Africa",
  "GA", "Central Africa",
  "ST", "Central Africa",

  # West Africa
  "BJ", "West Africa",
  "BF", "West Africa",
  "CV", "West Africa",
  "CI", "West Africa",
  "GM", "West Africa",
  "GH", "West Africa",
  "GN", "West Africa",
  "GW", "West Africa",
  "LR", "West Africa",
  "ML", "West Africa",
  "MR", "West Africa",
  "NE", "West Africa",
  "NG", "West Africa",
  "SN", "West Africa",
  "SL", "West Africa",
  "TG", "West Africa",

  # East Africa
  "BI", "East Africa",
  "KM", "East Africa",
  "DJ", "East Africa",
  "ER", "East Africa",
  "ET", "East Africa",
  "KE", "East Africa",
  "MG", "East Africa",
  "MW", "East Africa",
  "MU", "East Africa",
  "MZ", "East Africa",
  "RW", "East Africa",
  "SC", "East Africa",
  "SO", "East Africa",
  "SS", "East Africa",
  "SD", "East Africa",
  "TZ", "East Africa",
  "UG", "East Africa",
  "ZM", "East Africa",
  "ZW", "East Africa"
)

# ------------------------------------------------------------
# 4. Define destination macro-regions
# ------------------------------------------------------------

destination_groups <- tibble::tribble(
  ~destination_group, ~destination,

  # Europe
  "Europe", "AL", "Europe", "AD", "Europe", "AT", "Europe", "BE",
  "Europe", "BG", "Europe", "HR", "Europe", "CY", "Europe", "CZ",
  "Europe", "DK", "Europe", "EE", "Europe", "FI", "Europe", "FR",
  "Europe", "DE", "Europe", "GR", "Europe", "HU", "Europe", "IS",
  "Europe", "IE", "Europe", "IT", "Europe", "LV", "Europe", "LT",
  "Europe", "LU", "Europe", "MT", "Europe", "MD", "Europe", "ME",
  "Europe", "NL", "Europe", "MK", "Europe", "NO", "Europe", "PL",
  "Europe", "PT", "Europe", "RO", "Europe", "RS", "Europe", "SK",
  "Europe", "SI", "Europe", "ES", "Europe", "SE", "Europe", "CH",
  "Europe", "UA", "Europe", "GB",

  # US
  "US", "US",

  # LATAM
  "LATAM", "AR", "LATAM", "BO", "LATAM", "BR", "LATAM", "CL",
  "LATAM", "CO", "LATAM", "CR", "LATAM", "CU", "LATAM", "DO",
  "LATAM", "EC", "LATAM", "SV", "LATAM", "GT", "LATAM", "HN",
  "LATAM", "MX", "LATAM", "NI", "LATAM", "PA", "LATAM", "PY",
  "LATAM", "PE", "LATAM", "PR", "LATAM", "UY", "LATAM", "VE",

  # Asia
  "Asia", "CN", "Asia", "JP", "Asia", "KR", "Asia", "IN",
  "Asia", "ID", "Asia", "MY", "Asia", "PH", "Asia", "SG",
  "Asia", "TH", "Asia", "VN", "Asia", "PK", "Asia", "BD",
  "Asia", "LK", "Asia", "NP", "Asia", "IR", "Asia", "IQ",
  "Asia", "IL", "Asia", "JO", "Asia", "LB", "Asia", "SA",
  "Asia", "AE", "Asia", "QA", "Asia", "KW", "Asia", "TR",

  # Oceania
  "Oceania", "AU", "Oceania", "NZ", "Oceania", "FJ", "Oceania", "PG"
)

# ------------------------------------------------------------
# 5. Aggregate flows: African region -> destination bloc
# ------------------------------------------------------------

extra_africa_grouped <- country_flows_2025 %>%
  filter(
    origin %in% african_codes,
    !(destination %in% african_codes),
    origin != destination,
    weight > 0
  ) %>%
  left_join(origin_groups, by = "origin") %>%
  left_join(destination_groups, by = "destination") %>%
  filter(
    !is.na(origin_group),
    !is.na(destination_group)
  ) %>%
  group_by(origin_group, destination_group) %>%
  summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# 6. Artificial coordinates for groups
# ------------------------------------------------------------

origin_coords <- tibble::tribble(
  ~origin_group,      ~lon_origin, ~lat_origin,
  "Maghreb",              10,          30,
  "South Africa",         25,         -29,
  "Central Africa",       20,           2,
  "West Africa",          -5,           9,
  "East Africa",          38,           1
)

destination_coords <- tibble::tribble(
  ~destination_group, ~lon_dest, ~lat_dest,
  "Europe",              10,        50,
  "US",                -100,        38,
  "LATAM",              -60,       -15,
  "Asia",                95,        35,
  "Oceania",            135,       -25
)

extra_africa_map <- extra_africa_grouped %>%
  left_join(origin_coords, by = "origin_group") %>%
  left_join(destination_coords, by = "destination_group") %>%
  mutate(
    weight_scaled = scales::rescale(weight, to = c(0.25, 1))
  )

# ------------------------------------------------------------
# 7. Plot grouped Africa -> destination bloc flows
# ------------------------------------------------------------

p_extra_africa_groups <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey97",
    color = "grey78",
    linewidth = 0.20
  ) +

  geom_curve(
    data = extra_africa_map,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight_scaled,
      color = origin_group
    ),
    curvature = 0.25,
    arrow = arrow(length = unit(0.08, "inches"), type = "closed")
  ) +

  geom_point(
    data = origin_coords,
    aes(x = lon_origin, y = lat_origin),
    size = 2
  ) +

  geom_text(
    data = origin_coords,
    aes(x = lon_origin, y = lat_origin, label = origin_group),
    nudge_y = 4,
    size = 3.3,
    fontface = "bold"
  ) +

  geom_text(
    data = destination_coords,
    aes(x = lon_dest, y = lat_dest, label = destination_group),
    nudge_y = 5,
    size = 3.5,
    fontface = "bold"
  ) +

  scale_linewidth_continuous(
    range = c(0.20, 3.5),
    name = "Collaborations"
  ) +

  scale_alpha_continuous(
    range = c(0.20, 0.90),
    guide = "none"
  ) +

  labs(
    title = "Extra-African scientific collaboration flows by African region, 2025",
    subtitle = "Flows from African regional blocs to Europe, US, LATAM, Asia and Oceania",
    x = NULL,
    y = NULL,
    color = "African origin group"
  ) +

  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 80),
    expand = FALSE
  ) +

  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold")
  )

p_extra_africa_groups

ggsave(
  "extra_african_collaboration_flows_by_region_2025.jpeg",
  p_extra_africa_groups,
  width = 14,
  height = 8,
  dpi = 500
)




# ------------------------------------------------------------
# 7. Better “regional flow map” style
# ------------------------------------------------------------

# Optional: draw broad African subregion circles/polygons
africa_regions_bg <- tibble::tribble(
  ~origin_group,      ~lon, ~lat,
  "Maghreb",            10,  28,
  "West Africa",        -5,  10,
  "Central Africa",     20,   0,
  "East Africa",        38,   0,
  "South Africa",       25, -28
)

p_extra_africa_groups <- ggplot() +

  # world background
  geom_sf(
    data = world,
    fill = "grey96",
    color = "grey85",
    linewidth = 0.15
  ) +

  # soft background circles for African subregions
  geom_point(
    data = africa_regions_bg,
    aes(x = lon, y = lat, color = origin_group),
    size = 38,
    alpha = 0.12
  ) +

  # destination nodes
  geom_point(
    data = destination_coords,
    aes(x = lon_dest, y = lat_dest),
    size = 5.8,
    color = "grey15"
  ) +

  geom_text(
    data = destination_coords,
    aes(x = lon_dest, y = lat_dest, label = destination_group),
    nudge_y = 5,
    size = 4,
    fontface = "bold"
  ) +

  # origin nodes
  geom_point(
    data = origin_coords,
    aes(x = lon_origin, y = lat_origin, color = origin_group),
    size = 5.2
  ) +

  geom_text(
    data = origin_coords,
    aes(
      x = lon_origin,
      y = lat_origin,
      label = origin_group,
      color = origin_group
    ),
    nudge_y = 4,
    size = 3.8,
    fontface = "bold"
  ) +

  # collaboration flows
  geom_curve(
    data = extra_africa_map,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight_scaled,
      color = origin_group
    ),
    curvature = 0.22,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed"),
    lineend = "round"
  ) +

  scale_color_manual(
    values = c(
      "Maghreb" = "#7B3294",
      "West Africa" = "#1A9850",
      "Central Africa" = "#F28E1C",
      "East Africa" = "#2C7BB6",
      "South Africa" = "#D7191C"
    ),
    name = "African subregions"
  ) +

  scale_linewidth_continuous(
    range = c(0.25, 2.8),
    breaks = c(50, 100, 150, 200),
    name = "Collaborations"
  ) +

  scale_alpha_continuous(
    range = c(0.30, 0.85),
    guide = "none"
  ) +

  coord_sf(
    xlim = c(-170, 160),
    ylim = c(-55, 75),
    expand = FALSE
  ) +

  labs(
    title = "Extra-African scientific collaboration flows by African subregion, 2025",
    subtitle = "Flows from African subregions to the rest of the world (Europe, US, LATAM, Asia, Oceania)",
    caption = paste(
      "Notes: Arrows represent total scientific collaborations",
      "from each African subregion to major world regions.",
      "Thickness of arrows indicates the number of collaborations.",
      sep = "\n"
    ),
    x = NULL,
    y = NULL
  ) +

  guides(
    color = guide_legend(
      override.aes = list(size = 5, linewidth = 0)
    ),
    linewidth = guide_legend(
      override.aes = list(color = "grey20")
    )
  ) +

  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "grey35", hjust = 0.5),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0.78),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_extra_africa_groups

ggsave(
  "extra_african_collaboration_flows_by_subregion_2025.jpeg",
  p_extra_africa_groups,
  width = 14,
  height = 8,
  dpi = 500
)





# ------------------------------------------------------------
# 6bis. African subregion areas on the map
# ------------------------------------------------------------

africa_region_map <- world %>%
  mutate(country_code = iso_a2) %>%
  filter(country_code %in% african_codes) %>%
  left_join(origin_groups, by = c("country_code" = "origin")) %>%
  filter(!is.na(origin_group))

# ------------------------------------------------------------
# 7. Plot grouped Africa -> destination bloc flows
# ------------------------------------------------------------

p_extra_africa_groups <- ggplot() +

  geom_sf(
    data = world,
    fill = "grey96",
    color = "grey85",
    linewidth = 0.15
  ) +

  # African subregions filled by actual country areas
  geom_sf(
    data = africa_region_map,
    aes(fill = origin_group),
    color = NA,
    alpha = 0.25
  ) +

  geom_curve(
    data = extra_africa_map,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight_scaled,
      color = origin_group
    ),
    curvature = 0.22,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed"),
    lineend = "round"
  ) +

  geom_point(
    data = destination_coords,
    aes(x = lon_dest, y = lat_dest),
    size = 5.8,
    color = "grey15"
  ) +

  geom_text(
    data = destination_coords,
    aes(x = lon_dest, y = lat_dest, label = destination_group),
    nudge_y = 5,
    size = 4,
    fontface = "bold"
  ) +

  geom_point(
    data = origin_coords,
    aes(x = lon_origin, y = lat_origin, color = origin_group),
    size = 5.2
  ) +

  geom_text(
    data = origin_coords,
    aes(
      x = lon_origin,
      y = lat_origin,
      label = origin_group,
      color = origin_group
    ),
    nudge_y = 4,
    size = 3.8,
    fontface = "bold"
  ) +

  scale_color_manual(
    values = c(
      "Maghreb" = "#7B3294",
      "West Africa" = "#1A9850",
      "Central Africa" = "#F28E1C",
      "East Africa" = "#2C7BB6",
      "South Africa" = "#D7191C"
    ),
    name = "African subregions"
  ) +

  scale_fill_manual(
    values = c(
      "Maghreb" = "#7B3294",
      "West Africa" = "#1A9850",
      "Central Africa" = "#F28E1C",
      "East Africa" = "#2C7BB6",
      "South Africa" = "#D7191C"
    ),
    guide = "none"
  ) +

  scale_linewidth_continuous(
    range = c(0.25, 2.8),
    breaks = c(50, 100, 150, 200),
    name = "Collaborations"
  ) +

  scale_alpha_continuous(
    range = c(0.30, 0.85),
    guide = "none"
  ) +

  coord_sf(
    xlim = c(-170, 160),
    ylim = c(-55, 75),
    expand = FALSE
  )  +

  guides(
    color = guide_legend(
      override.aes = list(size = 5, linewidth = 0)
    ),
    linewidth = guide_legend(
      override.aes = list(color = "grey20")
    )
  ) +

  theme_void(base_size = 13) +
  theme(legend.position = 'none')

p_extra_africa_groups

ggsave('pextra.jpeg', p_extra_africa_groups, width = 16, height = 9, dpi =)