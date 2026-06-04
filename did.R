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
