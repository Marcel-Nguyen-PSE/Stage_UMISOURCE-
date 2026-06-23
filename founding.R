library(readr)
library(httr)
library(jsonlite)
library(dplyr)
library(stringdist)

# ---------------------------------------------------------------
# 1. Read the CSV and get unique institution names
# ---------------------------------------------------------------
uni <- read_csv("uni.csv")

# adjust "name" to whatever your institution column is actually called
insts <- uni %>% distinct(name) %>% pull(name)

# ---------------------------------------------------------------
# 2. ROR query function with exponential backoff
# ---------------------------------------------------------------
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

# ---------------------------------------------------------------
# 3. Loop with checkpointing
# ---------------------------------------------------------------
checkpoint_path <- "ror_checkpoint.rds"
results <- vector("list", length(insts))

for (i in seq_along(insts)) {
  results[[i]] <- query_ror(insts[i])
  Sys.sleep(0.3)

  if (i %% 50 == 0) {
    saveRDS(results, checkpoint_path)
    message(sprintf("Checkpoint: %d/%d", i, length(insts)))
  }
}

ror_results <- bind_rows(results)
saveRDS(ror_results, "ror_results_final.rds")

# ---------------------------------------------------------------
# 4. Flag weak matches
# ---------------------------------------------------------------
ror_results <- ror_results %>%
  mutate(needs_review = is.na(founded_date) | match_score < 0.85)

message(sprintf("%d / %d need manual review", sum(ror_results$needs_review), nrow(ror_results)))

# ---------------------------------------------------------------
# 5. Merge back into uni and export
# ---------------------------------------------------------------
uni <- uni %>%
  left_join(ror_results %>% select(query_name, founded_date),
             by = c("name" = "query_name"))

write_csv(uni, "uni_with_founded_date.csv")
write_csv(ror_results %>% filter(needs_review), "ror_needs_review.csv")

uni_names_missing <- uni %>%
  filter(is.na(founded_date.x)) %>%
  pull(name)

print(uni_names_missing)

uni <- uni %>% rename(founded_date = 'founded_date.x')

df_africa <- df_africa %>%
  left_join(uni, by = 'name')


df_founding <- df_africa %>%
  filter(
    !is.na(founded_date),
    year < founded_date,
    n_publication > 0
  )

n_distinct(df_founding$name)

df_africa <- df_africa %>% select(-researchers)

researchers <- WDI(
  country = "all",
  indicator = "SP.POP.SCIE.RD.P6",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) |>
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    researchers_per_million = SP.POP.SCIE.RD.P6
  )

df_africa <- df_africa |>
  left_join(researchers, by = c("country_code", "year"))



df_founding <- df_africa %>%
  filter(
    !is.na(founded_date),
    year < founded_date,
    n_publication > 0
  )

n_distinct(df_founding$name)

df_founding %>%
  group_by(name, founded_date) %>%
  summarise(
    first_pub_year = min(year[n_publication > 0]),
    total_pre_founding_pub = sum(n_publication),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pre_founding_pub))

df_gap <- df_africa %>%
  group_by(name) %>%
  summarise(
    founded_date = first(founded_date),
    first_pub_year = min(year[n_publication > 0], na.rm = TRUE)
  ) %>%
  mutate(
    gap = founded_date - first_pub_year
  ) %>%
  arrange(desc(gap))

hist(df_gap$gap)

write_csv(df_africa,'df_africa.csv')

df_gap %>%
  arrange(gap) %>%
  select(name, founded_date, first_pub_year, gap) %>%
  head(20)

df_gap <- df_africa %>%
  filter(n_publication > 0) %>%
  group_by(name) %>%
  summarise(
    founded_date = first(founded_date),
    first_pub_year = min(year),
    .groups = "drop"
  ) %>%
  mutate(
    gap = founded_date - first_pub_year
  )

uni_miss_v2 <- uni_miss_v2 %>% rename(founded_date.x = 'founded_date')

df_gap <- df_gap %>%
  filter(gap > 0)

uni_to_remove <- df_gap %>%
  filter(gap > 5) %>%
  pull(name)

df_africa <- df_africa %>%
  filter(!(name %in% uni_to_remove))

df_africa %>% filter(is.na(inst_id)) %>% distinct(name) %>% pull(name)
