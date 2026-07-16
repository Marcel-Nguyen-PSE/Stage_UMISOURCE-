target_year <- 2025
per_page <- 200
max_pages <- 1000

mailto <- "Marcel.Nguyen@ens.psl.eu"

african_codes <- c(
  "DZ", "AO", "BJ", "BW", "BF", "BI", "CV", "CM", "CF", "TD",
  "KM", "CG", "CD", "CI", "DJ", "EG", "GQ", "ER", "SZ", "ET",
  "GA", "GM", "GH", "GN", "GW", "KE", "LS", "LR", "LY", "MG",
  "MW", "ML", "MR", "MU", "MA", "MZ", "NA", "NE", "NG", "RW",
  "ST", "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "TZ", "TG",
  "TN", "UG", "ZM", "ZW"
)

country_flows_2025 <- read_csv('Data/country_flows_2025.csv')

# Data fetching process to get coauthorship of affiliated works (Very Long Process !!!!, jump to next section for computing) ----

fetch_openalex_page <- function(page, year = target_year, per_page = 200) {
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter = paste0(
        "authorships.institutions.continent:africa,",
        "publication_year:", as.integer(year)
      ),
      per_page = per_page,
      page = page,
      api_key = OPENALEX_API_KEY,
      mailto = mailto
    ) |>
    req_timeout(60)
  
  resp <- req_perform(req)
  
  jsonlite::fromJSON(
    resp_body_string(resp),
    simplifyVector = FALSE
  )
}

fetch_africa_works <- function(year = target_year, per_page = 200, max_pages = 1000) {
  out <- list()
  
  for (p in seq_len(max_pages)) {
    
    Sys.sleep(runif(1, 1.5, 3))
    
    message(sprintf("Fetching page %d", p))
    
    page_json <- tryCatch(
      fetch_openalex_page(
        page = p,
        year = year,
        per_page = per_page
      ),
      error = function(e) {
        message(sprintf(
          "[FETCH ERROR] page %d | %s",
          p,
          conditionMessage(e)
        ))
        NULL
      }
    )
    
    if (is.null(page_json)) break
    if (length(page_json$results) == 0) break
    
    page_df <- tryCatch(
      openalexR::oa2df(page_json$results, entity = "works"),
      error = function(e) {
        message(sprintf(
          "[oa2df ERROR] page %d | %s",
          p,
          conditionMessage(e)
        ))
        tibble()
      }
    )
    
    if (nrow(page_df) == 0) break
    
    out[[p]] <- page_df
    
    works_tmp <- bind_rows(out) |>
      distinct(id, .keep_all = TRUE)
    
    saveRDS(works_tmp, "africa_works_2025_progress.rds")
    
    if (nrow(page_df) < per_page) break
  }
  
  if (length(out) == 0) return(tibble())
  
  bind_rows(out) |>
    distinct(id, .keep_all = TRUE)
}

get_country_codes <- function(authorship_df) {
  
  tryCatch({
    
    a <- authorship_df
    
    if (is.null(a)) return(character(0))
    if (!is.data.frame(a)) return(character(0))
    if (nrow(a) == 0) return(character(0))
    
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        map(function(x) {
          if (is.null(x) || length(x) == 0) {
            NA_character_
          } else {
            as.character(x)
          }
        }) |>
        unlist(use.names = FALSE)
      
      return(unique(na.omit(countries)))
    }
    
    if ("affiliations" %in% names(a)) {
      countries <- a$affiliations |>
        map(function(x) {
          if (is.null(x)) return(NA_character_)
          if (!is.data.frame(x)) return(NA_character_)
          if (nrow(x) == 0) return(NA_character_)
          if (!"country_code" %in% names(x)) return(NA_character_)
          as.character(x$country_code)
        }) |>
        unlist(use.names = FALSE)
      
      return(unique(na.omit(countries)))
    }
    
    if ("institutions" %in% names(a)) {
      countries <- a$institutions |>
        map(function(x) {
          if (is.null(x)) return(NA_character_)
          if (!is.data.frame(x)) return(NA_character_)
          if (nrow(x) == 0) return(NA_character_)
          if (!"country_code" %in% names(x)) return(NA_character_)
          as.character(x$country_code)
        }) |>
        unlist(use.names = FALSE)
      
      return(unique(na.omit(countries)))
    }
    
    character(0)
    
  }, error = function(e) {
    character(0)
  })
}

make_africa_flows <- function(codes) {
  
  codes <- sort(unique(na.omit(as.character(codes))))
  
  african <- codes[codes %in% african_codes]
  partner <- codes[codes != ""]
  
  if (length(african) == 0 || length(partner) < 2) {
    return(tibble(
      origin = character(),
      destination = character()
    ))
  }
  
  expand_grid(
    origin = african,
    destination = partner
  ) |>
    filter(origin != destination)
}

africa_works_2025 <- fetch_africa_works(
  year = target_year,
  per_page = per_page,
  max_pages = max_pages
)

country_flows_raw_2025 <- africa_works_2025 |>
  select(work_id = id, authorships) |>
  mutate(
    country_codes = map(authorships, get_country_codes),
    flows = map(country_codes, make_africa_flows)
  ) |>
  select(work_id, flows) |>
  unnest(flows)

country_flows_2025 <- country_flows_raw_2025 |>
  distinct(work_id, origin, destination) |>
  count(origin, destination, name = "weight") |>
  arrange(desc(weight))

saveRDS(country_flows_raw_2025, "country_flows_raw_2025.rds")
saveRDS(country_flows_2025, "country_flows_2025.rds")

# Plotting of maps ---- 

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  st_make_valid()

origin_groups <- tribble(
  ~origin, ~origin_group,

  "DZ", "Maghreb", "MA", "Maghreb", "TN", "Maghreb",
  "LY", "Maghreb", "EG", "Maghreb",

  "ZA", "South Africa",

  "CM", "Central Africa", "CF", "Central Africa",
  "TD", "Central Africa", "CG", "Central Africa",
  "CD", "Central Africa", "GQ", "Central Africa",
  "GA", "Central Africa", "ST", "Central Africa",

  "BJ", "West Africa", "BF", "West Africa",
  "CV", "West Africa", "CI", "West Africa",
  "GM", "West Africa", "GH", "West Africa",
  "GN", "West Africa", "GW", "West Africa",
  "LR", "West Africa", "ML", "West Africa",
  "MR", "West Africa", "NE", "West Africa",
  "NG", "West Africa", "SN", "West Africa",
  "SL", "West Africa", "TG", "West Africa",

  "BI", "East Africa", "KM", "East Africa",
  "DJ", "East Africa", "ER", "East Africa",
  "ET", "East Africa", "KE", "East Africa",
  "MG", "East Africa", "MW", "East Africa",
  "MU", "East Africa", "MZ", "East Africa",
  "RW", "East Africa", "SC", "East Africa",
  "SO", "East Africa", "SS", "East Africa",
  "SD", "East Africa", "TZ", "East Africa",
  "UG", "East Africa", "ZM", "East Africa",
  "ZW", "East Africa"
)

destination_groups <- tribble(
  ~destination_group, ~destination,

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

  "US", "US",

  "LATAM", "AR", "LATAM", "BO", "LATAM", "BR", "LATAM", "CL",
  "LATAM", "CO", "LATAM", "CR", "LATAM", "CU", "LATAM", "DO",
  "LATAM", "EC", "LATAM", "SV", "LATAM", "GT", "LATAM", "HN",
  "LATAM", "MX", "LATAM", "NI", "LATAM", "PA", "LATAM", "PY",
  "LATAM", "PE", "LATAM", "PR", "LATAM", "UY", "LATAM", "VE",

  "Asia", "CN", "Asia", "JP", "Asia", "KR", "Asia", "IN",
  "Asia", "ID", "Asia", "MY", "Asia", "PH", "Asia", "SG",
  "Asia", "TH", "Asia", "VN", "Asia", "PK", "Asia", "BD",
  "Asia", "LK", "Asia", "NP", "Asia", "IR", "Asia", "IQ",
  "Asia", "IL", "Asia", "JO", "Asia", "LB", "Asia", "SA",
  "Asia", "AE", "Asia", "QA", "Asia", "KW", "Asia", "TR",

  "Oceania", "AU", "Oceania", "NZ",
  "Oceania", "FJ", "Oceania", "PG"
)

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
  summarise(
    weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

origin_coords <- tribble(
  ~origin_group,  ~lon_origin, ~lat_origin,
  "Maghreb",              10,          30,
  "South Africa",         25,         -29,
  "Central Africa",       20,           2,
  "West Africa",          -5,           9,
  "East Africa",          38,           1
)

destination_coords <- tribble(
  ~destination_group, ~lon_dest, ~lat_dest,
  "Europe",                  10,        50,
  "US",                    -100,        38,
  "LATAM",                  -60,       -15,
  "Asia",                    95,        35,
  "Oceania",                135,       -25
)

extra_africa_map <- extra_africa_grouped %>%
  left_join(origin_coords, by = "origin_group") %>%
  left_join(destination_coords, by = "destination_group") %>%
  mutate(
    weight_scaled = rescale(weight, to = c(0.25, 1))
  )

africa_region_map <- world %>%
  mutate(country_code = iso_a2) %>%
  filter(country_code %in% african_codes) %>%
  left_join(
    origin_groups,
    by = c("country_code" = "origin")
  ) %>%
  filter(!is.na(origin_group))

p_extra_africa_groups <- ggplot() +

  geom_sf(
    data = world,
    fill = "grey96",
    color = "grey85",
    linewidth = 0.15
  ) +

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
    arrow = arrow(
      length = unit(0.09, "inches"),
      type = "closed"
    ),
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
    aes(
      x = lon_dest,
      y = lat_dest,
      label = destination_group
    ),
    nudge_y = 5,
    size = 4,
    fontface = "bold"
  ) +

  geom_point(
    data = origin_coords,
    aes(
      x = lon_origin,
      y = lat_origin,
      color = origin_group
    ),
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
    )
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
    breaks = c(50, 100, 150, 200)
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

  theme_void(base_size = 13) +
  theme(
    legend.position = "none"
  )

ggsave(
  "Output/p_extra_africa.jpeg",
  p_extra_africa_groups,
  width = 16,
  height = 9,
  dpi = 300
)

africa_map <- world %>%
  filter(continent == "Africa")

centroids <- africa_map %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(
    country_code = iso_a2,
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(country_code, country_name = name, lon, lat)

intra_africa_all <- country_flows_2025 %>%
  filter(
    origin %in% african_codes,
    destination %in% african_codes,
    origin != destination,
    weight > 0
  )

n_top_origins <- 5

top_origin_codes <- intra_africa_all %>%
  group_by(origin) %>%
  summarise(total_collab = sum(weight), .groups = "drop") %>%
  arrange(desc(total_collab)) %>%
  slice_head(n = n_top_origins) %>%
  pull(origin)

intra_africa_all <- intra_africa_all %>%
  mutate(
    origin_group = if_else(
      origin %in% top_origin_codes,
      "Top origin countries",
      "Other African countries"
    )
  )

intra_africa_map <- intra_africa_all %>%
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

flows_rest <- intra_africa_map %>%
  filter(origin_group == "Other African countries")

flows_top <- intra_africa_map %>%
  filter(origin_group == "Top origin countries")

flows_rest <- flows_rest %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.2, 1)))

flows_top <- flows_top %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.2, 1)))


p_intra_africa_all <- ggplot() +
  geom_sf(
    data = africa_map,
    fill = "grey97",
    color = "grey78",
    linewidth = 0.25
  ) +
  
  # Other countries: blue gradient
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
    curvature = 0.22,
    arrow = arrow(length = unit(0.08, "inches"), type = "closed")
  ) +
  
  # Top countries: red gradient
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
    curvature = 0.22,
    arrow = arrow(length = unit(0.09, "inches"), type = "closed")
  ) +
  
  scale_linewidth_continuous(
    range = c(0.15, 3.2),
    name = "Collaborations"
  ) +
  scale_alpha_continuous(
    range = c(0.15, 0.9),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-20, 55),
    ylim = c(-37, 38),
    expand = FALSE
  )  +
  theme_void(base_size = 13) 

p_intra_africa_all

ggsave(
  "Output/intra_african_collaboration.jpeg",
  p_intra_africa_all,
  width = 16,
  height = 9,
  dpi = 500
)
