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

# ── Helper: count distinct countries among authorships of a work ─────────────
get_country_count <- function(countries_distinct_count, authorship_df) {
  tryCatch({
    if (!is.null(countries_distinct_count) && !is.na(countries_distinct_count)) {
      return(as.integer(countries_distinct_count))
    }

    a <- authorship_df
    if (is.null(a) || nrow(a) == 0) return(0L)

    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      n <- length(unique(na.omit(countries)))
      if (n > 0) return(n)
    }

    if ("institutions" %in% names(a)) {
      countries <- a$institutions |>
        purrr::map(function(x) {
          if (is.null(x) || nrow(x) == 0) return(NA_character_)
          x$country_code
        }) |>
        unlist()
      return(length(unique(na.omit(countries))))
    }

    NA_integer_
  }, error = function(e) NA_integer_)
}

# ── Fetch function — single attempt, no retries ──────────────────────────────
fetch_international_share_year <- function(inst_id, year) {
  Sys.sleep(runif(1, 1, 2))  # widened to ease rate-limit pressure

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
      message(sprintf("  [ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
      NULL
    }
  )

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

  result <- tryCatch({
    has_precomputed <- "countries_distinct_count" %in% names(works)

    works |>
      mutate(
        n_countries = purrr::map2_int(
          if (has_precomputed) countries_distinct_count else list(NULL)[rep(1, n())],
          authorships,
          get_country_count
        ),
        international = as.integer(n_countries >= 2)
      ) |>
      summarise(
        n_works              = n(),
        n_international      = sum(international, na.rm = TRUE),
        international_share  = n_international / n_works
      ) |>
      mutate(inst_id = inst_id, year = as.integer(year)) |>
      select(inst_id, year, n_works, n_international, international_share)
  }, error = function(e) {
    message(sprintf("  [PARSE ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
    tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = nrow(works), n_international = NA_integer_,
      international_share = NA_real_
    )
  })

  result
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
chunk_size <- 100
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



df_africa_founded <- df_africa %>%
  filter(year >= founded_date)
