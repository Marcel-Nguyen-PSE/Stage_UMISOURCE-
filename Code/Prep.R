library(progressr)
library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(plm)
library(openalexR)
library(purrr)
library(np)
library(furrr)
library(countrycode)
library(WDI)
library(typstable)
library(mgcv)
library(FactoMineR)
library(factoextra)

### Data fetching process 

# The final dataset can be loaded here 
df_africa <- read_csv('df_africa.csv')

# 1 : OpenAlex ID fetching 

options(
  openalex.mailto = "Marcel.Nguyen@ens.psl.eu"
)

# i) Function to get all African scientific hubs indexed in OpenAlex

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

# ii) Function to get OpenAlex ID

# The ID data set containing each institution + OpenAlex ID + Country code can be loaded here 
instidafrica <- read_csv('instidafrica.csv')

fetch_inst_id <- function(u) {
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
  result <- fetch_inst_id(u)
  inst_ids <- inst_ids %>%
    filter(name != u) %>%
    bind_rows(result)
  saveRDS(inst_ids, "inst_ids_africa_progress.rds")
}

df_africa <- df_africa %>%
  left_join(inst_ids, by = "name")

df_africa <- df_africa %>%
  mutate(
    country_code = countrycode(
      country_code,
      origin = "iso2c",
      destination = "iso3c"
    )
  )


# 2 : Publications/Citations/International Outlook data fetch 

# The dataset containing publications can be loaded here 
publications <- read_csv('pubs_africa.csv')

# The dataset containing citations can be loaded here 
citations <- read_csv('citations_africa.csv')

#The dataset containing IO variables can be loaded here 
international <- read_csv('results_inter.csv')

# i) Publications 

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

pubs <- if (file.exists("pubs_progress.rds")) readRDS("pubs_progress.rds") else tibble(inst_id = character(), year = integer(), n_publications = integer())

to_fetch <- df_africa %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(pubs, by = c("inst_id", "year")) 

for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_pubs(to_fetch$inst_id[i], to_fetch$year[i])
  pubs   <- bind_rows(pubs, result)
  saveRDS(pubs, "pubs_progress.rds")
}

df_africa <- df_africa %>%
  left_join(pubs, by = c('year', 'inst_id'))

# ii) Citations 

fetch_citations <- function(inst_id, year) {
  message("Fetching: ", inst_id, " | ", year)

  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 200,
      verbose          = FALSE
    ),
    error = function(e) NULL
  )

  n <- if (is.null(works) || nrow(works) == 0) NA_integer_ else sum(works$cited_by_count, na.rm = TRUE)
  tibble(inst_id = inst_id, year = as.integer(year), n_citations = n)
}

citations <- if (file.exists("citations_progress.rds")) {
  readRDS("citations_progress.rds")
} else {
  tibble(
    inst_id     = character(),
    year        = integer(),
    n_citations = integer()
  )
}

to_fetch <- tw_panel_merge %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(citations, by = c("inst_id", "year"))

for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_citations(
    to_fetch$inst_id[[i]],
    to_fetch$year[[i]]
  )
  citations <- bind_rows(citations, result)
  saveRDS(citations, "citations_progress.rds")
  Sys.sleep(1)
}

df_africa <- df_africa %>%
  left_join(citations, by = c('year', 'inst_id'))

# iii) International Outlook 

plan(multisession, workers = 2)

african_codes <- c(
  "DZ","AO","BJ","BW","BF","BI","CM","CV","CF","TD","KM","CG","CD","CI",
  "DJ","EG","GQ","ER","SZ","ET","GA","GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","YT","MA","MZ","NA","NE","NG","RE","RW",
  "SH","ST","SN","SC","SL","SO","ZA","SS","SD","TZ","TG","TN","UG","EH",
  "ZM","ZW"
)

get_country_codes <- function(authorship_df) {
  tryCatch({
    a <- authorship_df
    if (is.null(a) || nrow(a) == 0) return(character(0))
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      out <- unique(na.omit(countries))
      if (length(out) > 0) return(out)
    }
    if ("institutions" %in% names(a)) {
      countries <- a$institutions |>
        purrr::map(function(x) {
          if (is.null(x) || nrow(x) == 0) return(NA_character_)
          x$country_code
        }) |>
        unlist()
      return(unique(na.omit(countries)))
    }
    character(0)
  }, error = function(e) character(0))
}

classify_collab <- function(codes) {
  n_countries <- length(codes)
  if (n_countries == 0) {
    return(list(n_countries = NA_integer_, international_any = NA, inter_african = NA, extra_african = NA))
  }
  if (n_countries < 2) {
    return(list(n_countries = n_countries, international_any = FALSE, inter_african = FALSE, extra_african = FALSE))
  }
  has_non_african <- any(!(codes %in% african_codes))
  has_african_pair <- sum(codes %in% african_codes) >= 2  # at least 2 distinct African countries involved
  list(
    n_countries          = n_countries,
    international_any    = TRUE,                  # any cross-country collaboration at all
    inter_african         = has_african_pair,       # collaboration among ≥2 African countries (may also include non-African)
    extra_african          = has_non_african          # collaboration includes ≥1 country outside Africa
  )
}

fetch_international_share_year <- function(inst_id, year) {
  Sys.sleep(runif(1, 1, 3))
  n_count <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      options          = list(api_key = OPENALEX_API_KEY),
      count_only       = TRUE,
      verbose          = FALSE
    )$count
  )

  empty_row <- function(n_works = NA_integer_) {
    tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = n_works,
      n_international_any = NA_integer_, international_share_any = NA_real_,
      n_inter_african      = NA_integer_, inter_african_share      = NA_real_,
      n_extra_african       = NA_integer_, extra_african_share       = NA_real_
    )
  }
  if (is.na(n_count)) return(empty_row(NA_integer_))
  if (n_count == 0) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = 0L,
      n_international_any = 0L, international_share_any = NA_real_,
      n_inter_african      = 0L, inter_african_share      = NA_real_,
      n_extra_african       = 0L, extra_african_share       = NA_real_
    ))
  }
  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 100,
      pages            = "all",
      options          = list(api_key = OPENALEX_API_KEY),
      verbose          = FALSE
    )
  )
  if (is.null(works) || nrow(works) == 0) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = 0L,
      n_international_any = 0L, international_share_any = NA_real_,
      n_inter_african      = 0L, inter_african_share      = NA_real_,
      n_extra_african       = 0L, extra_african_share       = NA_real_
    ))
  }
  if (!"authorships" %in% names(works)) {
    return(empty_row(nrow(works)))
  }
  result <- tryCatch({
    works |>
      mutate(
        country_codes = purrr::map(authorships, get_country_codes),
        classification = purrr::map(country_codes, classify_collab),
        international_any = purrr::map_lgl(classification, ~ isTRUE(.x$international_any)),
        inter_african       = purrr::map_lgl(classification, ~ isTRUE(.x$inter_african)),
        extra_african        = purrr::map_lgl(classification, ~ isTRUE(.x$extra_african))
      ) |>
      summarise(
        n_works                  = n(),
        n_international_any      = sum(international_any, na.rm = TRUE),
        international_share_any  = n_international_any / n_works,
        n_inter_african           = sum(inter_african, na.rm = TRUE),
        inter_african_share       = n_inter_african / n_works,
        n_extra_african            = sum(extra_african, na.rm = TRUE),
        extra_african_share        = n_extra_african / n_works
      ) |>
      mutate(inst_id = inst_id, year = as.integer(year)) |>
      select(inst_id, year, n_works,
             n_international_any, international_share_any,
             n_inter_african, inter_african_share,
             n_extra_african, extra_african_share)
  })
}

to_fetch <- df_africa |>
  distinct(inst_id, year) |>
  filter(!is.na(inst_id))

results <- if (file.exists("international_share_year_progress.rds")) {
  readRDS("international_share_year_progress.rds") %>%
    filter(!is.na(n_works))
} else {
  tibble(
    inst_id = character(),
    year = integer(),
    n_works = integer(),
    n_international_any = integer(),
    international_share_any = numeric(),
    n_inter_african = integer(),
    inter_african_share = numeric(),
    n_extra_african = integer(),
    extra_african_share = numeric()
  )
}

to_fetch_remaining <- to_fetch |>
  anti_join(results, by = c("inst_id", "year"))

chunk_size <- 100
chunks <- split(
  to_fetch_remaining,
  ceiling(seq_len(nrow(to_fetch_remaining)) / chunk_size)
)

for (k in seq_along(chunks)) {
  chunk_result <- future_pmap_dfr(
    chunks[[k]],
    function(inst_id, year) {
      fetch_international_share_year(inst_id, year)
    },
    .options = furrr_options(seed = TRUE)
  )
  results <- bind_rows(results, chunk_result)
}

df_africa <- df_africa |>
  left_join(results, by = c("inst_id", "year"))

# 3 : Macroeconomic controls 

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

gdp_growth <- WDI(
  country = "all",
  indicator = "NY.GDP.MKTP.KD.ZG",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    gdp_growth = NY.GDP.MKTP.KD.ZG
  )

df_africa <- df_africa %>%
  left_join(
    gdp_growth,
    by = c("country_code", "year")
  )

internet <- WDI(
  country = "all",
  indicator = "IT.NET.USER.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    internet = IT.NET.USER.ZS
  )

df_africa <- df_africa %>%
  left_join(
    internet,
    by = c("country_code", "year")
  )

electricity <- WDI(
  country = "all",
  indicator = "EG.ELC.ACCS.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    electricity = EG.ELC.ACCS.ZS
  )

df_africa <- df_africa %>%
  left_join(
    electricity,
    by = c("country_code", "year")
  )

political_stab <- WDI(
  country = "all",
  indicator = "PV.EST",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    political_stab = PV.EST
  )

df_africa <- df_africa %>%
  left_join(
    political_stab,
    by = c("country_code", "year")
  )

tertiary_enrol <- WDI(
  country = "all",
  indicator = "SE.TER.ENRR",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    tertiary_enrol = SE.TER.ENRR
  )

df_africa <- df_africa %>%
  left_join(
    tertiary_enrol,
    by = c("country_code", "year")
  )

education_exp <- WDI(
  country = "all",
  indicator = "SE.XPD.TOTL.GD.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    education_exp = SE.XPD.TOTL.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    education_exp,
    by = c("country_code", "year")
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
    rd_exp = GB.XPD.RSDV.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    rd_exp,
    by = c("country_code", "year")
  )

researchers <- WDI(
  country = "all",
  indicator = "SP.POP.SCIE.RD.P6",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    researchers = SP.POP.SCIE.RD.P6
  )

df_africa <- df_africa %>%
  left_join(
    researchers,
    by = c("country_code", "year")
  )

patents <- WDI(
  country = "all",
  indicator = "IP.PAT.RESD",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    patents = IP.PAT.RESD
  )

df_africa <- df_africa %>%
  left_join(
    patents,
    by = c("country_code", "year")
  )

# 4 : Founding Date 

uni <- read_csv("uni.csv")

query_ror <- function(inst_name, max_retries = 5) {
  url <- "https://api.ror.org/organizations"
  attempt <- 1
  while (attempt <= max_retries) {
    res <- tryCatch(
      GET(url, query = list(query = inst_name), timeout(10)),
      error = function(e) NULL
    )
    if (!is.null(res) && status_code(res) == 200) {
      data <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
      if (length(data$items) == 0 || is.null(data$items)) {
        return(tibble(query_name = inst_name, ror_name = NA_character_,
                       founded_date = NA_integer_, match_score = NA_real_))
      }
      best  <- data$items[1, ]
      score <- 1 - stringdist(tolower(inst_name), tolower(best$name), method = "jw")
      return(tibble(
        query_name   = inst_name,
        ror_name     = best$name,
        founded_date = suppressWarnings(as.integer(best$established)),
        match_score  = round(score, 3)
      ))
    }
    Sys.sleep(2^attempt)
    attempt <- attempt + 1
  }
  tibble(query_name = inst_name, ror_name = NA_character_,
         founded_date = NA_integer_, match_score = NA_real_)
}

checkpoint_path <- "ror_checkpoint.rds"
results <- vector("list", length(insts))

for (i in seq_along(insts)) {
  results[[i]] <- query_ror(insts[i])
  Sys.sleep(0.3)
  if (i %% 50 == 0) {
    saveRDS(results, checkpoint_path)
  }
}

ror_results <- bind_rows(results)
saveRDS(ror_results, "ror_results_final.rds")

uni <- uni %>%
  left_join(ror_results %>% select(query_name, founded_date),
             by = c("name" = "query_name"))

df_founding <- df_africa %>%
  filter(
    !is.na(founded_date),
    year < founded_date,
    n_publication > 0
  )

