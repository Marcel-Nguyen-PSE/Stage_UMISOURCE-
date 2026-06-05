library(httr)
library(jsonlite)
library(countrycode)
library(WDI)

# Fetch African universities from OpenAlex
url <- "https://api.openalex.org/institutions"
all_names <- c()
page <- 1

repeat {
  res <- GET(url, query = list(
    filter = "continent:africa,type:education",
    select = "display_name",
    per_page = 200,
    page = page
  ))
  
  data <- fromJSON(rawToChar(res$content))
  results <- data$results$display_name
  
  if (length(results) == 0) break
  
  all_names <- c(all_names, results)
  page <- page + 1
}

df_africa <- tibble(
  name = all_names
) %>%
  crossing(
    year = 2006:2026
  )

########################################################

fetch_inst_id_safe <- function(u) {
  
  message("Fetching: ", u)
  
  inst <- tryCatch(
    oa_fetch(
      entity = "institutions",
      search = u,
      per_page = 1,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  Sys.sleep(0.5)
  
  tibble(
    name = u,
    inst_id = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$id[1],
    country_code = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$country_code[1],
    type = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$type[1]
  )
}

if (file.exists("inst_ids_africa_progress.rds")) {
  
  inst_ids <- readRDS("inst_ids_africa_progress.rds")
  
} else {
  
  inst_ids <- df_africa %>%
    distinct(name) %>%
    mutate(
      inst_id = NA_character_,
      country_code = NA_character_,
      type = NA_character_
    )
}

remaining_names <- inst_ids %>%
  filter(is.na(inst_id)) %>%
  pull(name)

for (u in remaining_names) {
  
  result <- fetch_inst_id_safe(u)
  
  inst_ids <- inst_ids %>%
    filter(name != u) %>%
    bind_rows(result)
  
  saveRDS(inst_ids, "inst_ids_africa_progress.rds")
}

df_africa <- read_csv('df_africa.csv') %>% select(name, year)

inst_ids <- read_csv('instidafrica.csv')

df_africa <- df_africa %>%
  left_join(inst_ids, by = "name")

write_csv(inst_ids, 'instidafrica.csv')
write_csv(df_africa, 'df_africa.csv')

df_africa <- read_csv('df_africa.csv')

inst_ids <- read_csv('instidafrica.csv') %>%
   mutate(inst_id = gsub("https://openalex.org/", "", inst_id))

df_africa <- df_africa %>%
  left_join(inst_ids, by = 'name')

df_africa <- df_africa %>%
  mutate(
    country_code = countrycode(
      country_code,
      origin = "iso2c",
      destination = "iso3c"
    )
  )

rd_exp <- WDI(
  country = "all",
  indicator = "GB.XPD.RSDV.GD.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    rd_exp_gdp = GB.XPD.RSDV.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    rd_exp,
    by = c("country_code", "year"))

ed_exp <- WDI(
  country = "all",
  indicator = "SE.XPD.TOTL.GD.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    ed_exp_gdp = SE.XPD.TOTL.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    ed_exp,
    by = c("country_code", "year")
  )

pat <- WDI(
  country = "all",
  indicator = "IP.PAT.RESD",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    pat = IP.PAT.RESD
  )

df_africa <- df_africa %>%
  left_join(
    pat,
    by = c("country_code", "year")
  )

net_us <- WDI(
  country = "all",
  indicator = "IT.NET.USER.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    net_us = IT.NET.USER.ZS
  )

df_africa <- df_africa %>%
  left_join(
    net_us,
    by = c("country_code", "year")
  )

gdp_cap <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    gdp_cap = NY.GDP.PCAP.KD
  )

df_africa <- df_africa %>%
  left_join(
    gdp_cap,
    by = c("country_code", "year")
  )

write_csv(df_africa, 'df_africa.csv')


df_africa <- read_csv('df_africa.csv')
inst_ids <- read_csv('instidafrica.csv')

fetch_pubs <- function(inst_id, year, retries = 3) {
  for (i in seq_len(retries)) {
    Sys.sleep(0.5)
    works <- tryCatch(
      oa_fetch(
        entity           = "works",
        institutions.id  = inst_id,
        publication_year = year,
        count_only       = TRUE,
        verbose          = FALSE
      ),
      error = function(e) NULL
    )
    if (!is.null(works)) break
    Sys.sleep(5 * i)
  }
  n <- if (is.null(works)) NA_integer_ else as.integer(works$count[1])
  tibble(inst_id = inst_id, year = year, n_publications = n)
}

# Resume from checkpoint if exists
pubs <- if (file.exists("pubs_progress.rds")) readRDS("pubs_progress.rds") else tibble(inst_id = character(), year = integer(), n_publications = integer())

# Build the full list of (inst_id, year) pairs to fetch
to_fetch <- df_africa %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(pubs, by = c("inst_id", "year"))  # skip already fetched

message(nrow(to_fetch), " requests remaining")
  
for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_pubs(to_fetch$inst_id[i], to_fetch$year[i])
  pubs   <- bind_rows(pubs, result)
  saveRDS(pubs, "pubs_progress.rds")
}

write_csv(pubs, 'pubs_africa.csv')

df_africa <- df_africa %>%
  left_join(pubs, by = c('year', 'inst_id'))

write_csv(df_africa, 'df_africa.csv')

panel_kernel_africa <- df_africa %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    research_t  = log1p(n_publications),
    research_t1 = dplyr::lead(log1p(n_publications)),
    research_t6 = dplyr::lead(log1p(n_publications), 6)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(research_t),
    !is.na(research_t1),
    !is.na(research_t6)
  )

kernel_model <- npreg(
  research_t6 ~ research_t,
  data = panel_kernel_africa
)

grid <- data.frame(
  research_t = seq(
    min(panel_kernel_africa$research_t),
    max(panel_kernel_africa$research_t),
    length.out = 300
  )
)

grid$research_t1_hat <- predict(kernel_model, newdata = grid)

ggplot(grid, aes(x = research_t, y = research_t1_hat)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "Research stock at t: log(1 + publications)",
    y = "Research stock at t+1: log(1 + publications)",
    title = "Kernel-estimated research transition function"
  ) +
  theme_minimal()

df_africa <- df_africa %>%
  rename(n_publication = 'n_publications')

# Select year
year_plot <- 2006

# Aggregate publications by country
pub_country <- df_africa %>%
  filter(year == year_plot) %>%
  group_by(country_code) %>%
  summarise(publications = sum(n_publication, na.rm = TRUE))

# African country boundaries
africa_map <- rnaturalearth::ne_countries(
  continent = "Africa",
  returnclass = "sf"
)

# Merge map with publication data
africa_map <- africa_map %>%
  left_join(
    pub_country,
    by = c("iso_a3" = "country_code")
  )

# Plot
pub_2006 <- ggplot(africa_map) +
  geom_sf(aes(fill = publications), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Publications"
  ) +
  labs(
    title = paste("Publications by Country in", year_plot)
  ) +
  theme_minimal()

ggsave('pub_afr_2006.jpeg', pub_2006)

# Aggregate publications by country
pub_country_2025 <- df_africa %>%
  filter(year == 2025) %>%
  group_by(country_code) %>%
  summarise(publications = sum(n_publication, na.rm = TRUE))

# African country boundaries
africa_map <- rnaturalearth::ne_countries(
  continent = "Africa",
  returnclass = "sf"
)

# Merge map with publication data
africa_map_2025 <- africa_map %>%
  left_join(
    pub_country_2025,
    by = c("iso_a3" = "country_code")
  )

# Plot
pub_2025 <- ggplot(africa_map_2025) +
  geom_sf(aes(fill = publications), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Publications"
  ) +
  labs(
    title = paste("Publications by Country in", 2025)
  ) +
  theme_minimal()

ggsave('pub_afr_2025.jpeg', pub_2025)

library(patchwork)

combined_map <- pub_2006 + pub_2025 +
  plot_layout(ncol = 2)

ggsave(
  filename = "publications_africa_2006_2025.jpeg",
  plot = combined_map,
  width = 14,
  height = 7,
  dpi = 300
)

country_year <- df_africa %>%
  group_by(country_code, year) %>%
  summarise(
    total_publications = sum(n_publication, na.rm = TRUE),
    .groups = "drop"
  )

pub_year_afr <- ggplot(country_year,
       aes(x = year,
           y = total_publications,
           color = country_code,
           group = country_code)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Publications by Country Over Time",
    x = "Year",
    y = "Total Publications",
    color = "Country"
  ) +
  theme_minimal()

ggsave('pub_year_afr.jpeg', pub_year_afr, width = 16, height = 9)

library(dplyr)

# ACE 1 — treatment year: 2014
ace1_universities <- c(
  "Redeemer's University",
  "University of Port Harcourt",
  "Ahmadu Bello University",
  "Obafemi Awolowo University",
  "African University of Science and Technology",
  "University of Jos",
  "University of Benin",
  "Federal University of Agriculture Abeokuta",
  "Bayero University",
  "Benue State University",
  "University of Ghana",
  "Kwame Nkrumah University of Science and Technology",
  "Institut National Polytechnique Félix Houphouët-Boigny",
  "Université Félix Houphouët-Boigny",
  "École Nationale Supérieure de Statistique et d'Économie Appliquée",
  "Université Gaston Berger",
  "Université Cheikh Anta Diop",
  "Université d'Abomey-Calavi",
  "Institut International d'Ingénierie de l'Eau et de l'Environnement",
  "Université de Yaoundé I",
  "Université de Lomé",
  "University of The Gambia"
)

# ACE 2 — treatment year: 2016
ace2_universities <- c(
  "Addis Ababa University",
  "Haramaya University",
  "Egerton University",
  "Moi University",
  "Jaramogi Oginga Odinga University of Science and Technology",
  "Lilongwe University of Agriculture and Natural Resources",
  "University of Malawi",
  "Eduardo Mondlane University",
  "University of Rwanda",
  "Nelson Mandela African Institution of Science and Technology",
  "Sokoine University of Agriculture",
  "Makerere University",
  "Mbarara University of Science and Technology",
  "Uganda Martyrs University",
  "Copperbelt University",
  "University of Zambia"
)

df_africa <- df_africa %>%
  mutate(
    ace_1 = as.integer(name %in% ace1_universities & year >= 2014),
    ace_2 = as.integer(name %in% ace2_universities & year >= 2016)
  )

el_acc <- WDI(
  country = "all",
  indicator = "EG.ELC.ACCS.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    el_acc = EG.ELC.ACCS.ZS
  )

df_africa <- df_africa %>%
  left_join(
    el_acc,
    by = c("country_code", "year"))

res_lev <- WDI(
  country = "all",
  indicator = "SP.POP.SCIE.RD.P6",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    res_lev = SP.POP.SCIE.RD.P6
  )

df_africa <- df_africa %>%
  left_join(
    res_lev,
    by = c("country_code", "year"))

did_main <- feols(
  n_publication ~ ace_1 + ace_2 |
    name + year + country_code[year],
  cluster = ~ country_code,
  data = df_africa
)

summary(did_main)

es_ace1 <- feols(
  n_publication ~ i(year, ace_1, ref = 2013) |
    name + year + country_code[year],
  cluster = ~country_code,
  data = df_africa
)

library(ggfixest)

library(ggfixest)
library(patchwork)

p1 <- ggiplot(es_ace1) +
  ggtitle("ACE 1")

p2 <- ggiplot(es_ace2) +
  ggtitle("ACE 2")

combined_did <- p1 + p2 +
  plot_layout(ncol = 2)

ggsave(
  "event_studies_ace1_ace2.jpeg",
  combined_did,
  width = 12,
  height = 6,
  dpi = 300
)

iplot(es_ace1)

es_ace2 <- feols(
  n_publication ~ i(year, ace_2, ref = 2015) |
    name + year + country_code[year],
  cluster = ~country_code,
  data = df_africa
)

iplot(es_ace2)

df_africa <- df_africa |>
  group_by(name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    cum_publications = cumsum(n_publication)
  ) |>
  ungroup()

df_africa_pt <- df_africa |>
  group_by(name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    log_stock = log1p(cum_publications),
    stock_t5 = dplyr::lead(cum_publications, 5),
    log_stock_t5 = log1p(stock_t5)
  ) |>
  ungroup() |>
  filter(
    !is.na(log_stock),
    !is.na(log_stock_t5)
  )

m_stock <- feols(
  log_stock_t5 ~ bs(log_stock, df = 4),
  data = df_africa_pt
)

grid <- data.frame(
  log_stock = seq(
    min(df_africa_pt$log_stock),
    max(df_africa_pt$log_stock),
    length.out = 500
  )
)

grid$pred <- predict(m_stock, newdata = grid)

ggplot(grid, aes(log_stock, pred)) +
  geom_line(size = 1) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Log cumulative publications",
    y = "Expected log cumulative publications in t+5"
  ) + theme_minimal()

inflation <- WDI(
  country = "all",
  indicator = "FP.CPI.TOTL.ZG",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    inflation = FP.CPI.TOTL.ZG
  )

df_africa <- df_africa %>%
  left_join(
    inflation,
    by = c("country_code", "year")) 

write_csv(df_africa, 'df_africa.csv')

deltas_1 <- c(
  # WACCBIP — University of Ghana, Legon
  "University of Ghana",

  # MARCAD — Université Cheikh Anta Diop, Dakar
  "Université Cheikh Anta Diop",

  # DELGEME — University of Science, Techniques and Technology of Bamako
  "Université des Sciences des Techniques et des Technologies de Bamako",

  # SANTHE — Africa Health Research Institute / University of KwaZulu-Natal
  "University of KwaZulu-Natal",

  # MUII-Plus — Makerere University / Uganda Virus Research Institute
  "Makerere University",

  # AMARI — University of Zimbabwe
  "University of Zimbabwe",

  # THRiVE-2 — Makerere University College of Health Sciences
  # (same legal entity as Makerere University above — no duplicate needed)

  # CARTA+ — African Population and Health Research Center (APHRC)
  # APHRC is a research centre, not a university; match both common forms
  "African Population and Health Research Center",
  "African Population and Health Research Centre",

  # IDeAL — KEMRI-Wellcome Trust Research Programme, Kilifi
  "Kenya Medical Research Institute",
  "KEMRI-Wellcome Trust Research Programme",

  # Afrique One-ASPIRE — Centre Suisse de Recherches Scientifiques, Abidjan
  "Centre Suisse de Recherches Scientifiques en Côte d'Ivoire",

  # SSACAB — University of the Witwatersrand
  "University of the Witwatersrand"
)

df_africa <- df_africa %>%
  mutate(
    deltas_1 = as.integer(name %in% deltas_1 & year >= 2015))

did_deltas <- feols(
  n_publication ~ deltas_1 |
    name + year + country_code[year],
  cluster = ~ country_code,
  data = df_africa
)

summary(did_main)

es_deltas1 <- feols(
  n_publication ~ i(year, deltas_1, ref = 2013) |
    name + year + country_code[year],
  cluster = ~country_code,
  data = df_africa
)

iplot(es_deltas1)
########################################

# 1. Build clean wide publication matrix
pub_mat <- df_africa |>
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

# 2. Force numeric year columns
pub_mat <- pub_mat |>
  mutate(
    across(-name, as.numeric)
  )

# 3. Check structure
str(pub_mat)
sapply(pub_mat, class)

# 4. Define data columns
data_cols <- 2:ncol(pub_mat)

# 5. Run log-t convergence test
logt <- logtTest(
  pub_mat,
  dataCols = data_cols,
  unit_names = 1
)

summary(logt)

# 6. Identify convergence clubs
clubs <- findClubs(
  pub_mat,
  dataCols = data_cols,
  unit_names = 1
)

summary(clubs)

# 7. Extract club membership
club_members <- clubs$club.data

club_members

# 8. Plot clubs
plot(clubs)


pub_mat <- df_africa |>
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
  arrange(name) |>
  mutate(
    across(-name, as.numeric)
  )

pub_mat <- pub_mat |>
  mutate(
    across(-name, ~ log1p(.x))
  )

clubs <- findClubs(
  pub_mat,
  dataCols = 2:ncol(pub_mat),
  unit_names = pub_mat$name,
  refCol = ncol(pub_mat)
)

summary(clubs)




library(dplyr)
library(tidyr)
library(ConvergenceClubs)

# 1. Build wide data
pub_mat <- df_africa |>
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

# 6. Check
stopifnot(is.character(pub_mat$name))
stopifnot(all(sapply(pub_mat[year_cols], is.numeric)))

# 7. Run club convergence
clubs <- findClubs(
  pub_mat,
  dataCols = year_cols,
  unit_names = 1,
  refCol = max(year_cols)
)

summary(clubs)
plot(clubs)
str(clubs)

club_data <- attr(clubs, "data")
data_cols <- attr(clubs, "dataCols")
year_cols <- names(club_data)[data_cols]

club_members <- bind_rows(
  lapply(seq_along(clubs), function(i) {
    data.frame(
      name = clubs[[i]]$unit_names,
      club = paste0("club", i)
    )
  })
)

pub_long <- club_data |>
  select(name, all_of(year_cols)) |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(
    time = as.integer(str_remove(year, "^y"))
  )

pub_long <- pub_long |>
  group_by(time) |>
  mutate(
    h_it = value / mean(value, na.rm = TRUE)
  ) |>
  ungroup()

club_paths <- pub_long |>
  inner_join(club_members, by = "name") |>
  group_by(club, time) |>
  summarise(
    avg_transition_path = mean(h_it, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(club_paths, aes(
  x = time,
  y = avg_transition_path,
  color = club,
  shape = club,
  linetype = club
)) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Average transition paths – All clubs",
    x = "Time",
    y = "Relative transition path",
    color = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

club_members <- bind_rows(
  lapply(seq_along(clubs), function(i) {
    data.frame(
      name = clubs[[i]]$unit_names,
      club = paste0("club", i)
    )
  })
)

df_africa |>
  filter(year == 2025) |>
  left_join(club_members, by = "name") |>
  ggplot(aes(
    x = log1p(n_publication),
    fill = club,
    colour = club
  )) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Publication density by convergence club (2025)",
    x = "log(1 + publications)",
    y = "Density"
  ) +
  theme_minimal()

df_africa |>
  filter(year == 2006) |>
  left_join(club_members, by = "name") |>
  ggplot(aes(
    x = log1p(n_publication),
    fill = club,
    colour = club
  )) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Publication density by convergence club (2006)",
    x = "log(1 + publications)",
    y = "Density"
  ) +
  theme_minimal()



library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)

# Average transition path plot
p_avg_path <- ggplot(club_paths, aes(
  x = time,
  y = avg_transition_path,
  color = club,
  shape = club,
  linetype = club
)) +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Average transition paths – All clubs",
    x = "Time",
    y = "Relative transition path",
    color = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Density plot: 2006
p_density_2006 <- df_africa |>
  filter(year == 2006) |>
  left_join(club_members, by = "name") |>
  ggplot(aes(
    x = log1p(n_publication),
    fill = club,
    colour = club
  )) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Publication density by club (2006)",
    x = "log(1 + publications)",
    y = "Density",
    fill = NULL,
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Density plot: 2025
p_density_2025 <- df_africa |>
  filter(year == 2025) |>
  left_join(club_members, by = "name") |>
  ggplot(aes(
    x = log1p(n_publication),
    fill = club,
    colour = club
  )) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Publication density by club (2025)",
    x = "log(1 + publications)",
    y = "Density",
    fill = NULL,
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Combine: average path on top, densities below
combined_plot <- p_avg_path / (p_density_2006 + p_density_2025) +
  plot_layout(
    heights = c(1.1, 1),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

# Export single JPEG
ggsave(
  filename = "average_path_and_densities.jpeg",
  plot = combined_plot,
  width = 14,
  height = 10,
  dpi = 300
)

df_panel <- read_csv('df_panel.csv') 

nrow(df_panel %>% filter(country_code %in% african_codes) %>% distinct(name))

fepois(
  citations ~ publications +
    ace1 + ace2 +
    delta_1 + net_us +
    gdp_cap |
    name + year,
  cluster = ~country_code,
  data = df_africa
)

library(dplyr)
library(FactoMineR)
library(factoextra)

# 1. Une ligne par université pour une année donnée
df_acm <- df_africa |>
  filter(year == 2025) |>
  left_join(club_members, by = "name") |>
  mutate(
    ace1_cat = factor(ifelse(ace_1 == 1, "ACE1", "No ACE1")),
    ace2_cat = factor(ifelse(ace_2 == 1, "ACE2", "No ACE2")),
    pub_q = factor(ntile(n_publication, 4), labels = c("Pub Q1", "Pub Q2", "Pub Q3", "Pub Q4")),
    cit_q = factor(ntile(n_citation, 4), labels = c("Cit Q1", "Cit Q2", "Cit Q3", "Cit Q4")),
    gdp_q = factor(ntile(gdp_cap, 4), labels = c("GDP Q1", "GDP Q2", "GDP Q3", "GDP Q4")),
    net_q = factor(ntile(net_us, 4), labels = c("Net Q1", "Net Q2", "Net Q3", "Net Q4")),
    club = factor(club)
  ) |>
  select(
    ace1_cat,
    ace2_cat,
    pub_q,
    cit_q,
    gdp_q,
    net_q,
    club
  ) |>
  na.omit()

# 2. ACM
# club est en 7e colonne, donc quali.sup = 7
res_acm <- MCA(
  df_acm,
  quali.sup = 7,
  graph = FALSE
)

# 3. Graphique des modalités actives + clubs supplémentaires
p_acm <- fviz_mca_var(
  res_acm,
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = c("grey70", "steelblue", "darkred")
) +
  labs(
    title = "ACM des profils universitaires africains",
    subtitle = "Clubs de convergence projetés comme variables supplémentaires"
  ) +
  theme_minimal()

p_acm

ggsave(
  "acm_profils_universitaires.jpeg",
  p_acm,
  width = 10,
  height = 8,
  dpi = 300
)

library(dplyr)
library(ggplot2)
library(np)

# 1. Build 5-year transitions: t -> t+5
trans_pub_5 <- df_africa |>
  arrange(name, year) |>
  group_by(name) |>
  mutate(
    p_t  = log1p(n_publication),
    p_t5 = dplyr::lead(log1p(n_publication), 5),
    year_t5 = dplyr::lead(year, 5)
  ) |>
  ungroup() |>
  filter(
    !is.na(p_t),
    !is.na(p_t5),
    year_t5 == year + 5
  )

# 2. Nonparametric local linear regression
bw_5 <- npreg(
  p_t5 ~ p_t,
  data = trans_pub_5,
  regtype = "ll"
)

fit_5 <- npreg(
  bw_5,
  newdata = data.frame(
    p_t = seq(
      min(trans_pub_5$p_t, na.rm = TRUE),
      max(trans_pub_5$p_t, na.rm = TRUE),
      length.out = 300
    )
  )
)

# 3. Extract fitted transition function
transition_5_df <- data.frame(
  p_t = fit_5$eval[, "p_t"],
  p_t5_hat = fitted(fit_5)
)

# 4. Plot transition function with 45-degree line
p_transition_5 <- ggplot() +
  geom_point(
    data = trans_pub_5,
    aes(x = p_t, y = p_t5),
    alpha = 0.04,
    size = 0.6
  ) +
  geom_line(
    data = transition_5_df,
    aes(x = p_t, y = p_t5_hat),
    linewidth = 1.2,
    color = "red"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Five-year publication transition function",
    x = "log(1 + publications) at t",
    y = "Expected log(1 + publications) at t + 5"
  ) +
  theme_minimal(base_size = 14)

p_transition_5

