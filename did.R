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
