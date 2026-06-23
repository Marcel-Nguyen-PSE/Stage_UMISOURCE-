library(dplyr)
library(purrr)
library(furrr)
library(future)
library(openalexR)
library(readr)

# ── Settings ──────────────────────────────────────────────────────────────────
OPENALEX_API_KEY <- "RNnWtjEHVZgXnaQzeN1KuT"
options(openalexR.mailto = "Marcel.Nguyen@ens.psl.eu")  # also set this for the polite pool
plan(multisession, workers = 4)

# ── Helper: count distinct countries in an authorship list ───────────────────
get_country_count <- function(a) {
  if (is.null(a) || nrow(a) == 0) return(0L)
  countries <- a$affiliations |>
    purrr::map(function(x) {
      if (is.null(x) || nrow(x) == 0) return(NA_character_)
      x$country_code
    }) |>
    unlist()
  length(unique(na.omit(countries)))
}

# ── Fetch function with retries (429 + timeout handling) ─────────────────────
fetch_international_share_year <- function(inst_id, year, retries = 5) {
  Sys.sleep(runif(1, 0.3, 0.6))
  
  works <- NULL
  for (i in seq_len(retries)) {
    works <- tryCatch(
      oa_fetch(
        entity           = "works",
        institutions.id  = inst_id,
        publication_year = as.integer(year),
        per_page         = 200,
        pages            = "all",
        options          = list(api_key = OPENALEX_API_KEY),
        verbose          = FALSE
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429", msg)) {
          wait <- 2^i + runif(1, 0, 2)
          message(sprintf("  [429] waiting %.1fs (retry %d/%d) | %s %d",
                          wait, i, retries, inst_id, year))
          Sys.sleep(wait)
        } else if (grepl("Timeout|timed out", msg, ignore.case = TRUE)) {
          wait <- 2^i + runif(1, 0, 2)
          message(sprintf("  [TIMEOUT] waiting %.1fs (retry %d/%d) | %s %d",
                          wait, i, retries, inst_id, year))
          Sys.sleep(wait)
        } else {
          message(sprintf("  [ERROR] %s | %s %d", msg, inst_id, year))
          Sys.sleep(5)
        }
        NULL
      }
    )
    if (!is.null(works)) break
  }
  
  if (is.null(works)) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = NA_integer_, n_international = NA_integer_,
      international_share = NA_real_
    ))
  }
  
  if (nrow(works) == 0) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = 0L, n_international = 0L,
      international_share = NA_real_
    ))
  }
  
  if (!"authorships" %in% names(works)) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = nrow(works), n_international = NA_integer_,
      international_share = NA_real_
    ))
  }
  
  works |>
    mutate(
      n_countries   = map_int(authorships, get_country_count),
      international = as.integer(n_countries >= 2)
    ) |>
    summarise(
      n_works              = n(),
      n_international      = sum(international, na.rm = TRUE),
      international_share  = n_international / n_works
    ) |>
    mutate(inst_id = inst_id, year = as.integer(year)) |>
    select(inst_id, year, n_works, n_international, international_share)
}

# ── Resume from checkpoint ────────────────────────────────────────────────────
to_fetch <- df_africa |>
  distinct(inst_id, year) |>
  filter(!is.na(inst_id))

results <- if (file.exists("international_share_year_progress.rds")) {
  message("Resuming from checkpoint...")
  readRDS("international_share_year_progress.rds") %>%
    filter(!is.na(n_works))  # drop failed rows so they get retried
} else {
  tibble(
    inst_id = character(),
    year = integer(),
    n_works = integer(),
    n_international = integer(),
    international_share = numeric()
  )
}

to_fetch_remaining <- to_fetch |>
  anti_join(results, by = c("inst_id", "year"))

message(sprintf("%d requests remaining", nrow(to_fetch_remaining)))

# ── Chunked parallel loop ─────────────────────────────────────────────────────
chunk_size <- 100  # smaller chunks = more frequent checkpoints
chunks <- split(
  to_fetch_remaining,
  ceiling(seq_len(nrow(to_fetch_remaining)) / chunk_size)
)

for (k in seq_along(chunks)) {
  message(sprintf("Chunk %d / %d", k, length(chunks)))
  
  chunk_result <- future_pmap_dfr(
    chunks[[k]],
    function(inst_id, year) {
      fetch_international_share_year(inst_id, year)
    },
    .options = furrr_options(seed = TRUE)
  )
  
  results <- bind_rows(results, chunk_result)
  saveRDS(results, "international_share_year_progress.rds")
  write_csv(results, "international_share_year.csv")
  
  message(sprintf("  Done | total fetched: %d | successful: %d | failed: %d",
                  nrow(results),
                  sum(!is.na(results$n_works)),
                  sum(is.na(results$n_works))))
}

# ── Done ──────────────────────────────────────────────────────────────────────
df_africa <- df_africa |>
  left_join(results, by = c("inst_id", "year"))

plan(sequential)