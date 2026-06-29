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


library(openalexR)
library(purrr)
library(dplyr)

# ── Step 1: inspect actual structure for one institution-year ────────────────
sample <- oa_fetch(
  entity           = "works",
  institutions.id  = "I4210123613",   # replace with a real inst_id from df_africa
  publication_year = 2020,
  per_page         = 3,
  options          = list(api_key = OPENALEX_API_KEY)
)

names(sample)                              # check if countries_distinct_count exists
str(sample$authorships[[1]], max.level = 3) # confirm nested field names

# ── Step 2: robust country-count function ────────────────────────────────────
# Tries the OpenAlex-precomputed field first (countries_distinct_count),
# falls back to authorships$countries, then authorships$institutions$country_code.
# affiliations$institution_ids is NOT used -- affiliations has no country_code.

get_country_count <- function(work_row) {
  tryCatch({

    # Option A: precomputed field on the work itself
    if (!is.null(work_row$countries_distinct_count) &&
        !is.na(work_row$countries_distinct_count)) {
      return(as.integer(work_row$countries_distinct_count))
    }

    a <- work_row$authorships[[1]]
    if (is.null(a) || nrow(a) == 0) return(0L)

    # Option B: authorships$countries (list-column, already per-author country codes)
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      n <- length(unique(na.omit(countries)))
      if (n > 0) return(n)
    }

    # Option C: authorships$institutions[[i]]$country_code
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

# ── Step 3: apply row-wise (needs the whole work row, not just authorships) ──
works <- sample |>
  rowwise() |>
  mutate(n_countries = get_country_count(pick(everything()))) |>
  ungroup() |>
  mutate(international = as.integer(n_countries >= 2))

works |> select(id, display_name, n_countries, international)



library(openalexR)
library(purrr)
library(dplyr)

# ── Step 1: inspect actual structure for one institution-year ────────────────
sample <- oa_fetch(
  entity           = "works",
  institutions.id  = "I4210123613",   # replace with a real inst_id from df_africa
  publication_year = 2020,
  per_page         = 3,
  options          = list(api_key = OPENALEX_API_KEY)
)

names(sample)                              # check if countries_distinct_count exists
str(sample$authorships[[1]], max.level = 3) # confirm nested field names

# ── Step 2: robust country-count function ────────────────────────────────────
# Tries the OpenAlex-precomputed field first (countries_distinct_count),
# falls back to authorships$countries, then authorships$institutions$country_code.
# affiliations$institution_ids is NOT used -- affiliations has no country_code.

get_country_count <- function(work_row) {
  tryCatch({

    # Option A: precomputed field on the work itself
    if (!is.null(work_row$countries_distinct_count) &&
        !is.na(work_row$countries_distinct_count)) {
      return(as.integer(work_row$countries_distinct_count))
    }

    a <- work_row$authorships[[1]]
    if (is.null(a) || nrow(a) == 0) return(0L)

    # Option B: authorships$countries (list-column, already per-author country codes)
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      n <- length(unique(na.omit(countries)))
      if (n > 0) return(n)
    }

    # Option C: authorships$institutions[[i]]$country_code
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

# ── Step 3: apply row-wise (needs the whole work row, not just authorships) ──
works <- sample |>
  rowwise() |>
  mutate(n_countries = get_country_count(pick(everything()))) |>
  ungroup() |>
  mutate(international = as.integer(n_countries >= 2))

works |> select(id, display_name, n_countries, international)




library(dplyr)
library(purrr)
library(furrr)
library(future)
library(openalexR)
library(readr)

# ── Settings ──────────────────────────────────────────────────────────────────
# SECURITY: this key was pasted in plaintext in a previous version of this script.
# Rotate/regenerate it on https://openalex.org/settings/api, then load it like this:
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY")
options(openalexR.mailto = "Marcel.Nguyen@ens.psl.eu")  # also set this for the polite pool
plan(multisession, workers = 4)

# ── Helper: count distinct countries among authorships of a work ─────────────
# FIX: the original version looked for country_code inside `authorships$affiliations`,
# but affiliations elements only contain raw_affiliation_string + institution_ids,
# never country_code. That's why n_countries was always 0/NA.
# This version tries, in order:
#   A) the work-level precomputed field `countries_distinct_count` (fastest, no parsing)
#   B) authorships$countries (per-author country codes, already flattened by OpenAlex)
#   C) authorships$institutions[[i]]$country_code (dehydrated Institution objects)
get_country_count <- function(countries_distinct_count, authorship_df) {
  tryCatch({
    # Option A
    if (!is.null(countries_distinct_count) && !is.na(countries_distinct_count)) {
      return(as.integer(countries_distinct_count))
    }

    a <- authorship_df
    if (is.null(a) || nrow(a) == 0) return(0L)

    # Option B
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        purrr::map(function(x) if (is.null(x) || length(x) == 0) NA_character_ else x) |>
        unlist()
      n <- length(unique(na.omit(countries)))
      if (n > 0) return(n)
    }

    # Option C
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

  # FIX: wrap the whole transformation in tryCatch so a structural surprise
  # (e.g. a missing field, an unexpected NULL) degrades to one NA row instead
  # of crashing the entire chunk inside future_pmap_dfr().
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




library(openalexR)
library(purrr)
library(dplyr)

# ── Diagnostic script: isolate where the error actually occurs ───────────────
inst_id <- "I4210155196"
year    <- 2006

cat("══ STEP 1: Build the query ═══════════════════════════════════════\n")
q <- tryCatch({
  oa_query(
    entity = "works",
    filter = list(
      "institutions.id"  = inst_id,
      "publication_year" = year
    ),
    options = list(api_key = OPENALEX_API_KEY)
  )
}, error = function(e) { cat("FAILED at oa_query:\n"); print(e); NULL })

if (!is.null(q)) cat("Query URL:\n", q, "\n\n")

cat("══ STEP 2: Raw request (list/JSON, no df conversion) ═══════════════\n")
raw <- tryCatch({
  oa_request(
    query_url = q,
    count_only = FALSE,
    verbose    = TRUE
  )
}, error = function(e) { cat("FAILED at oa_request:\n"); print(e); NULL })

if (!is.null(raw)) {
  cat("Raw result class:", class(raw), "\n")
  cat("Raw result length:", length(raw), "\n")
  cat("Names of first element:\n")
  print(names(raw[[1]]))
  cat("\nStructure of meta/count info (if present):\n")
  str(raw$meta, max.level = 2)
}

cat("\n══ STEP 3: Convert to dataframe (oa2df) ═════════════════════════════\n")
df_out <- tryCatch({
  oa2df(raw, entity = "works")
}, error = function(e) {
  cat("FAILED at oa2df:\n")
  print(e)
  cat("\nFull call stack at failure:\n")
  print(sys.calls())
  NULL
})

if (!is.null(df_out)) {
  cat("Success! Dimensions:", dim(df_out), "\n")
  cat("Column types:\n")
  print(sapply(df_out, class))
}

cat("\n══ STEP 4: Inspect authorships/countries_distinct_count structure ═══\n")
if (!is.null(df_out)) {
  cat("Has countries_distinct_count?", "countries_distinct_count" %in% names(df_out), "\n")
  if ("countries_distinct_count" %in% names(df_out)) {
    cat("Class:", class(df_out$countries_distinct_count), "\n")
    print(head(df_out$countries_distinct_count))
  }
  if ("authorships" %in% names(df_out) && length(df_out$authorships) > 0) {
    cat("\nauthorships[[1]] structure:\n")
    str(df_out$authorships[[1]], max.level = 3)
  }
}

cat("\n══ STEP 5: Try the full oa_fetch with verbose, capture warnings too ═\n")
withCallingHandlers(
  tryCatch({
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 200,
      pages            = "all",
      options          = list(api_key = OPENALEX_API_KEY),
      verbose          = TRUE
    )
  }, error = function(e) {
    cat("FAILED at oa_fetch (full pipeline):\n")
    cat("Message:", conditionMessage(e), "\n")
    cat("Call:", deparse(conditionCall(e)), "\n")
    NULL
  }),
  warning = function(w) {
    cat("WARNING during oa_fetch:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }
)







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

# ── Fetch function — checks count first to avoid the openalexR zero-result bug ─
fetch_international_share_year <- function(inst_id, year) {
  Sys.sleep(runif(1, 1, 2))

  # Step 1: cheap count-only check. Sidesteps the pages="all" bug entirely
  # when there are zero matching works (the actual root cause of the crash).
  n_count <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      options          = list(api_key = OPENALEX_API_KEY),
      count_only       = TRUE,
      verbose          = FALSE
    )$count,
    error = function(e) {
      message(sprintf("  [COUNT ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
      NA_integer_
    }
  )

  if (is.na(n_count)) {
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = NA_integer_, n_international = NA_integer_,
      international_share = NA_real_
    ))
  }

  if (n_count == 0) {
    # Known zero -- skip pages="all" fetch entirely, avoids the openalexR bug
    return(tibble(
      inst_id = inst_id, year = as.integer(year),
      n_works = 0L, n_international = 0L,
      international_share = NA_real_
    ))
  }

  # Step 2: only fetch full records when we know there's at least 1 result
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

  if (is.null(works) || nrow(works) == 0) {
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
    filter(!is.na(n_works))
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













library(dplyr)
library(purrr)
library(furrr)
library(future)
library(openalexR)
library(readr)

df_africa <- read_csv('df_africa.csv')

# ── Settings ──────────────────────────────────────────────────────────────────
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY")  # rotate the previously exposed key
options(openalexR.mailto = "Marcel.Nguyen@ens.psl.eu")
plan(multisession, workers = 2)

# ── ISO 3166-1 alpha-2 codes for African countries ───────────────────────────
african_codes <- c(
  "DZ","AO","BJ","BW","BF","BI","CM","CV","CF","TD","KM","CG","CD","CI",
  "DJ","EG","GQ","ER","SZ","ET","GA","GM","GH","GN","GW","KE","LS","LR",
  "LY","MG","MW","ML","MR","MU","YT","MA","MZ","NA","NE","NG","RE","RW",
  "SH","ST","SN","SC","SL","SO","ZA","SS","SD","TZ","TG","TN","UG","EH",
  "ZM","ZW"
)

# ── Helper: get distinct country codes for one work's authorships ────────────
# Returns the full set of country codes (not just a count), so we can later
# classify the collaboration as inter-African vs extra-African vs domestic-only.
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

# ── Helper: classify a work's collaboration type from its country codes ─────
classify_collab <- function(codes) {
  n_countries <- length(codes)

  if (n_countries == 0) {
    return(list(n_countries = NA_integer_, international_any = NA, inter_african = NA, extra_african = NA))
  }
  if (n_countries < 2) {
    # single country -- domestic, not international by any definition
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

# ── Fetch function — checks count first to avoid the openalexR zero-result bug ─
fetch_international_share_year <- function(inst_id, year) {
  Sys.sleep(runif(1, 1, 3))

  # Step 1: cheap count-only check. Sidesteps the pages="all" bug entirely
  # when there are zero matching works (the actual root cause of the crash).
  n_count <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      options          = list(api_key = OPENALEX_API_KEY),
      count_only       = TRUE,
      verbose          = FALSE
    )$count,
    error = function(e) {
      message(sprintf("  [COUNT ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
      NA_integer_
    }
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

  # Step 2: only fetch full records when we know there's at least 1 result
  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 100,
      pages            = "all",
      options          = list(api_key = OPENALEX_API_KEY),
      verbose          = FALSE
    ),
    error = function(e) {
      message(sprintf("  [ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
      NULL
    }
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
  }, error = function(e) {
    message(sprintf("  [PARSE ERROR] %s | %s %d", conditionMessage(e), inst_id, year))
    empty_row(nrow(works))
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





# ── Robust helper: country codes from authorships ─────────────────────────────

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
      
      out <- unique(na.omit(countries))
      if (length(out) > 0) return(out)
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
      
      out <- unique(na.omit(countries))
      if (length(out) > 0) return(out)
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
      
      out <- unique(na.omit(countries))
      if (length(out) > 0) return(out)
    }
    
    character(0)
    
  }, error = function(e) {
    character(0)
  })
}

library(dplyr)

library(purrr)

library(openalexR)

library(tibble)

fetch_works_manual_pages <- function(inst_id, year, per_page = 200, max_pages = 100) {

  

  pages_list <- list()

  

  for (p in seq_len(max_pages)) {

    

    Sys.sleep(runif(1, 3, 6))

    

    page <- tryCatch(

      oa_fetch(

        entity = "works",

        institutions.id = inst_id,

        publication_year = as.integer(year),

        per_page = per_page,

        pages = p,

        options = list(api_key = OPENALEX_API_KEY),

        verbose = FALSE

      ),

      error = function(e) {

        message(sprintf(

          "[PAGE FETCH ERROR] page %d | %s %d | %s",

          p, inst_id, year, conditionMessage(e)

        ))

        NULL

      }

    )

    

    if (is.null(page)) break

    if (nrow(page) == 0) break

    

    pages_list[[p]] <- page

    

    message(sprintf(

      "Fetched page %d | %s %d | rows: %d",

      p, inst_id, year, nrow(page)

    ))

    

    if (nrow(page) < per_page) break

  }

  

  if (length(pages_list) == 0) {

    return(tibble())

  }

  

  bind_rows(pages_list)

} 

# ── Helper: classify collaboration type ───────────────────────────────────────

classify_collab <- function(codes) {
  
  codes <- unique(na.omit(as.character(codes)))
  n_countries <- length(codes)
  
  if (n_countries == 0) {
    return(list(
      n_countries = NA_integer_,
      international_any = NA,
      inter_african = NA,
      extra_african = NA
    ))
  }
  
  if (n_countries < 2) {
    return(list(
      n_countries = n_countries,
      international_any = FALSE,
      inter_african = FALSE,
      extra_african = FALSE
    ))
  }
  
  has_non_african <- any(!(codes %in% african_codes))
  has_african_pair <- sum(codes %in% african_codes) >= 2
  
  list(
    n_countries = n_countries,
    international_any = TRUE,
    inter_african = has_african_pair,
    extra_african = has_non_african
  )
}

# ── Empty output row ──────────────────────────────────────────────────────────

empty_collab_row <- function(inst_id, year, n_works = NA_integer_) {
  tibble(
    inst_id = inst_id,
    year = as.integer(year),
    n_works = n_works,
    n_international_any = NA_integer_,
    international_share_any = NA_real_,
    n_inter_african = NA_integer_,
    inter_african_share = NA_real_,
    n_extra_african = NA_integer_,
    extra_african_share = NA_real_
  )
}

zero_collab_row <- function(inst_id, year) {
  tibble(
    inst_id = inst_id,
    year = as.integer(year),
    n_works = 0L,
    n_international_any = 0L,
    international_share_any = NA_real_,
    n_inter_african = 0L,
    inter_african_share = NA_real_,
    n_extra_african = 0L,
    extra_african_share = NA_real_
  )
}

# ── Main fetch function ───────────────────────────────────────────────────────

fetch_international_share_year <- function(inst_id, year) {
  
  works <- fetch_works_manual_pages(
    inst_id = inst_id,
    year = year,
    per_page = 200,
    max_pages = 100
  )
  
  if (is.null(works)) {
    return(empty_collab_row(inst_id, year, NA_integer_))
  }
  
  if (nrow(works) == 0) {
    return(zero_collab_row(inst_id, year))
  }
  
  if (!"authorships" %in% names(works)) {
    return(empty_collab_row(inst_id, year, nrow(works)))
  }
  
  result <- tryCatch({
    
    works |>
      mutate(
        country_codes = map(authorships, get_country_codes),
        classification = map(country_codes, classify_collab),
        
        international_any = map_lgl(
          classification,
          ~ isTRUE(.x$international_any)
        ),
        
        inter_african = map_lgl(
          classification,
          ~ isTRUE(.x$inter_african)
        ),
        
        extra_african = map_lgl(
          classification,
          ~ isTRUE(.x$extra_african)
        )
      ) |>
      summarise(
        n_works = n(),
        
        n_international_any = sum(international_any, na.rm = TRUE),
        international_share_any = n_international_any / n_works,
        
        n_inter_african = sum(inter_african, na.rm = TRUE),
        inter_african_share = n_inter_african / n_works,
        
        n_extra_african = sum(extra_african, na.rm = TRUE),
        extra_african_share = n_extra_african / n_works
      ) |>
      mutate(
        inst_id = inst_id,
        year = as.integer(year)
      ) |>
      select(
        inst_id,
        year,
        n_works,
        n_international_any,
        international_share_any,
        n_inter_african,
        inter_african_share,
        n_extra_african,
        extra_african_share
      )
    
  }, error = function(e) {
    
    message(sprintf(
      "[PARSE ERROR] %s | %s %d",
      conditionMessage(e),
      inst_id,
      year
    ))
    
    empty_collab_row(inst_id, year, nrow(works))
  })
  
  result
}

# ── Resume from checkpoint ────────────────────────────────────────────────────

to_fetch <- df_africa |>
  distinct(inst_id, year) |>
  filter(!is.na(inst_id))

results <- if (file.exists("international_share_year_progress.rds")) {
  
  message("Resuming from checkpoint...")
  
  readRDS("international_share_year_progress.rds") |>
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

message(sprintf("%d requests remaining", nrow(to_fetch_remaining)))

# ── Chunked loop ──────────────────────────────────────────────────────────────
# Start sequential for stability. Switch to workers = 2 only after testing.

plan(sequential)

chunk_size <- 50

chunks <- split(
  to_fetch_remaining,
  ceiling(seq_len(nrow(to_fetch_remaining)) / chunk_size)
)

for (k in seq_along(chunks)) {
  
  message(sprintf("Chunk %d / %d", k, length(chunks)))
  
  chunk_result <- pmap_dfr(
    chunks[[k]],
    function(inst_id, year) {
      
      tryCatch(
        fetch_international_share_year(inst_id, year),
        error = function(e) {
          
          message(sprintf(
            "[FATAL ERROR] %s | %s %d",
            conditionMessage(e),
            inst_id,
            year
          ))
          
          empty_collab_row(inst_id, year, NA_integer_)
        }
      )
    }
  )
  
  results <- bind_rows(results, chunk_result)
  
  saveRDS(results, "international_share_year_progress.rds")
  write_csv(results, "international_share_year.csv")
  
  message(sprintf(
    "Done | total fetched: %d | successful: %d | failed: %d",
    nrow(results),
    sum(!is.na(results$n_works)),
    sum(is.na(results$n_works))
  ))
}

# ── Merge back ────────────────────────────────────────────────────────────────

df_africa <- df_africa |>
  left_join(results, by = c("inst_id", "year"))

plan(sequential)


oa_fetch(
  entity = "works",
  institutions.id = "I4405255384",
  publication_year = 2018,
  per_page = 5,
  pages = 1,
  options = list(api_key = OPENALEX_API_KEY)
)



write_csv(results, 'results_inter.csv')







library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(openalexR)
library(readr)

OPENALEX_API_KEY <- "YOUR_API_KEY"

fetch_openalex_page <- function(page, per_page = 200) {
  
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter = "authorships.institutions.continent:africa,publication_year:2025",
      per_page = per_page,
      page = page,
      api_key = OPENALEX_API_KEY,
      mailto = "Marcel.Nguyen@ens.psl.eu"
    ) |>
    req_timeout(60)
  
  resp <- req_perform(req)
  jsonlite::fromJSON(resp_body_string(resp), simplifyVector = FALSE)
}

fetch_africa_works_2025 <- function(per_page = 200, max_pages = 2000, retries = 5) {
  
  out <- list()
  
  for (p in seq_len(max_pages)) {
    
    Sys.sleep(runif(1, 1.5, 3))
    
    page_json <- NULL
    
    for (attempt in seq_len(retries)) {
      
      page_json <- tryCatch(
        fetch_openalex_page(p, per_page),
        error = function(e) {
          wait <- min(180, 10 * 2^(attempt - 1)) + runif(1, 0, 5)
          message(sprintf(
            "[Retry %d/%d] page %d | %s | waiting %.1fs",
            attempt, retries, p, conditionMessage(e), wait
          ))
          Sys.sleep(wait)
          NULL
        }
      )
      
      if (!is.null(page_json)) break
    }
    
    if (is.null(page_json)) break
    if (length(page_json$results) == 0) break
    
    page_df <- openalexR::oa2df(page_json$results, entity = "works")
    
    out[[p]] <- page_df
    
    message(sprintf(
      "Fetched page %d | rows: %d | total so far: %d",
      p, nrow(page_df), sum(map_int(out, nrow))
    ))
    
    saveRDS(bind_rows(out), "africa_works_2025_progress.rds")
    
    if (nrow(page_df) < per_page) break
  }
  
  bind_rows(out) |>
    distinct(id, .keep_all = TRUE)
}

get_country_codes <- function(authorship_df) {
  
  if (is.null(authorship_df)) return(character(0))
  if (!is.data.frame(authorship_df)) return(character(0))
  if (!"affiliations" %in% names(authorship_df)) return(character(0))
  
  countries <- authorship_df$affiliations |>
    map(function(x) {
      if (is.null(x)) return(NA_character_)
      if (!is.data.frame(x)) return(NA_character_)
      if (!"country_code" %in% names(x)) return(NA_character_)
      as.character(x$country_code)
    }) |>
    unlist(use.names = FALSE)
  
  unique(na.omit(countries))
}

make_country_pairs <- function(codes) {
  
  codes <- sort(unique(na.omit(codes)))
  
  if (length(codes) < 2) {
    return(tibble(country_1 = character(), country_2 = character()))
  }
  
  pairs <- combn(codes, 2)
  
  tibble(
    country_1 = pairs[1, ],
    country_2 = pairs[2, ]
  )
}

africa_works_2025 <- fetch_africa_works_2025()

country_edges_2025 <- africa_works_2025 |>
  select(work_id = id, authorships) |>
  mutate(
    country_codes = map(authorships, get_country_codes),
    country_pairs = map(country_codes, make_country_pairs)
  ) |>
  select(work_id, country_pairs) |>
  unnest(country_pairs)

country_flows_2025 <- country_edges_2025 |>
  count(country_1, country_2, name = "weight") |>
  arrange(desc(weight))

write_csv(country_flows_2025, "country_flows_2025.csv")
saveRDS(country_flows_2025, "country_flows_2025.rds")








library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(httr2)
library(jsonlite)
library(openalexR)
library(readr)
library(future)
library(furrr)

# ============================================================
# 0. Settings
# ============================================================

OPENALEX_API_KEY <- "YOUR_API_KEY"

options(openalexR.mailto = "Marcel.Nguyen@ens.psl.eu")
options(openalexR.apikey = OPENALEX_API_KEY)

target_year <- 2025

plan(sequential)

# ============================================================
# 1. Fetch one OpenAlex REST page properly
# ============================================================

fetch_works_page <- function(inst_id, year, page = 1, per_page = 200) {
  
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter = paste0(
        "institutions.id:", inst_id,
        ",publication_year:", as.integer(year)
      ),
      per_page = per_page,
      page = page,
      api_key = OPENALEX_API_KEY,
      mailto = "Marcel.Nguyen@ens.psl.eu"
    ) |>
    req_timeout(60)
  
  resp <- req_perform(req)
  txt <- resp_body_string(resp)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# ============================================================
# 2. Fetch all works for one institution-year with retries
# ============================================================

fetch_works_inst_year <- function(inst_id, year, per_page = 200, max_pages = 100, retries = 5) {
  
  pages_list <- list()
  
  for (p in seq_len(max_pages)) {
    
    Sys.sleep(runif(1, 2, 5))
    
    page_json <- NULL
    
    for (attempt in seq_len(retries)) {
      
      page_json <- tryCatch(
        fetch_works_page(
          inst_id = inst_id,
          year = year,
          page = p,
          per_page = per_page
        ),
        error = function(e) {
          
          wait <- min(180, 10 * 2^(attempt - 1)) + runif(1, 0, 5)
          
          message(sprintf(
            "[RETRY %d/%d] page %d | %s %d | %s | waiting %.1fs",
            attempt, retries, p, inst_id, year, conditionMessage(e), wait
          ))
          
          Sys.sleep(wait)
          NULL
        }
      )
      
      if (!is.null(page_json)) break
    }
    
    if (is.null(page_json)) {
      message(sprintf(
        "[FAILED PAGE] %s %d page %d",
        inst_id, year, p
      ))
      break
    }
    
    results_raw <- page_json$results
    
    if (length(results_raw) == 0) break
    
    page_df <- tryCatch(
      openalexR::oa2df(results_raw, entity = "works"),
      error = function(e) {
        message(sprintf(
          "[oa2df ERROR] %s %d page %d | %s",
          inst_id, year, p, conditionMessage(e)
        ))
        tibble()
      }
    )
    
    if (nrow(page_df) == 0) break
    
    pages_list[[p]] <- page_df
    
    message(sprintf(
      "Fetched page %d | %s %d | rows: %d",
      p, inst_id, year, nrow(page_df)
    ))
    
    if (nrow(page_df) < per_page) break
  }
  
  if (length(pages_list) == 0) return(tibble())
  
  bind_rows(pages_list) |>
    distinct(id, .keep_all = TRUE)
}

# ============================================================
# 3. Extract country codes from authorships
# ============================================================

get_country_codes <- function(authorship_df) {
  
  tryCatch({
    
    a <- authorship_df
    
    if (is.null(a)) return(character(0))
    if (!is.data.frame(a)) return(character(0))
    if (nrow(a) == 0) return(character(0))
    
    if ("countries" %in% names(a)) {
      countries <- a$countries |>
        map(function(x) {
          if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
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

# ============================================================
# 4. Convert country codes into country-pair edges
# ============================================================

make_country_pairs <- function(codes) {
  
  codes <- sort(unique(na.omit(as.character(codes))))
  
  if (length(codes) < 2) {
    return(tibble(
      country_1 = character(),
      country_2 = character()
    ))
  }
  
  pairs <- combn(codes, 2)
  
  tibble(
    country_1 = pairs[1, ],
    country_2 = pairs[2, ]
  )
}

# ============================================================
# 5. Fetch collaboration edges for one institution-year
# ============================================================

fetch_country_edges_inst_year <- function(inst_id, year) {
  
  works <- fetch_works_inst_year(
    inst_id = inst_id,
    year = year,
    per_page = 200,
    max_pages = 100
  )
  
  if (is.null(works) || nrow(works) == 0) {
    return(tibble())
  }
  
  if (!"authorships" %in% names(works)) {
    return(tibble())
  }
  
  edges <- works |>
    select(work_id = id, authorships) |>
    mutate(
      country_codes = map(authorships, get_country_codes),
      country_pairs = map(country_codes, make_country_pairs)
    ) |>
    select(work_id, country_pairs) |>
    unnest(country_pairs) |>
    mutate(
      inst_id = inst_id,
      year = as.integer(year)
    )
  
  edges
}

# ============================================================
# 6. Prepare 2025 institution list
# ============================================================

to_fetch_2025 <- df_africa |>
  filter(year == target_year) |>
  distinct(inst_id) |>
  filter(!is.na(inst_id))

# Resume checkpoint if available
country_edges_2025 <- if (file.exists("country_edges_2025_progress.rds")) {
  message("Resuming from checkpoint...")
  readRDS("country_edges_2025_progress.rds")
} else {
  tibble(
    work_id = character(),
    country_1 = character(),
    country_2 = character(),
    inst_id = character(),
    year = integer()
  )
}

already_done <- country_edges_2025 |>
  distinct(inst_id)

to_fetch_remaining <- to_fetch_2025 |>
  anti_join(already_done, by = "inst_id")

message(sprintf(
  "%d institutions remaining for %d",
  nrow(to_fetch_remaining),
  target_year
))

# ============================================================
# 7. Sequential fetching loop with checkpointing
# ============================================================

chunk_size <- 25

chunks <- split(
  to_fetch_remaining,
  ceiling(seq_len(nrow(to_fetch_remaining)) / chunk_size)
)

for (k in seq_along(chunks)) {
  
  message(sprintf("Chunk %d / %d", k, length(chunks)))
  
  chunk_edges <- pmap_dfr(
    chunks[[k]],
    function(inst_id) {
      
      tryCatch(
        fetch_country_edges_inst_year(inst_id, target_year),
        error = function(e) {
          message(sprintf(
            "[FATAL ERROR] %s | %s %d",
            conditionMessage(e),
            inst_id,
            target_year
          ))
          tibble()
        }
      )
    }
  )
  
  country_edges_2025 <- bind_rows(country_edges_2025, chunk_edges) |>
    distinct(work_id, country_1, country_2, .keep_all = TRUE)
  
  saveRDS(country_edges_2025, "country_edges_2025_progress.rds")
  write_csv(country_edges_2025, "country_edges_2025_raw.csv")
  
  message(sprintf(
    "Saved checkpoint | total country-pair edges: %d",
    nrow(country_edges_2025)
  ))
}

# ============================================================
# 8. Aggregate country-country flows
# ============================================================

country_flows_2025 <- country_edges_2025 |>
  count(country_1, country_2, name = "weight") |>
  arrange(desc(weight))

write_csv(country_flows_2025, "country_flows_2025.csv")
saveRDS(country_flows_2025, "country_flows_2025.rds")

country_flows_2025
















df_africa <- read_csv('df_africa.csv')




library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(httr2)
library(jsonlite)
library(openalexR)
library(readr)

# ============================================================
# 0. Settings
# ============================================================

OPENALEX_API_KEY <- "YOUR_API_KEY"

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

# ============================================================
# 1. Fetch one page from OpenAlex REST API
# ============================================================

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

# ============================================================
# 2. Fetch all Africa-affiliated works in 2025 — no retries
# ============================================================

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
    
    message(sprintf(
      "Page %d saved | rows: %d | total unique works: %d",
      p,
      nrow(page_df),
      nrow(works_tmp)
    ))
    
    if (nrow(page_df) < per_page) break
  }
  
  if (length(out) == 0) return(tibble())
  
  bind_rows(out) |>
    distinct(id, .keep_all = TRUE)
}

# ============================================================
# 3. Extract country codes from authorships
# ============================================================

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

# ============================================================
# 4. Convert countries into Africa-to-partner flows
# ============================================================

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

# ============================================================
# 5. Run fetch
# ============================================================

africa_works_2025 <- fetch_africa_works(
  year = target_year,
  per_page = per_page,
  max_pages = max_pages
)

saveRDS(africa_works_2025, "africa_works_2025.rds")
write_csv(africa_works_2025, "africa_works_2025.csv")

# ============================================================
# 6. Build collaboration-flow edge list
# ============================================================

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

write_csv(country_flows_raw_2025, "country_flows_raw_2025.csv")
write_csv(country_flows_2025, "country_flows_2025.csv")

saveRDS(country_flows_raw_2025, "country_flows_raw_2025.rds")
saveRDS(country_flows_2025, "country_flows_2025.rds")

country_flows_2025





library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)
library(readr)

# country_flows_2025 must have:
# origin | destination | weight
# Example: ZA | US | 1234

# ============================================================
# 1. Country centroids
# ============================================================

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
  select(country_code, name, lon, lat)

# ============================================================
# 2. Prepare flow coordinates
# ============================================================

flows_map <- country_flows_2025 %>%
  filter(weight > 0) %>%
  left_join(
    centroids %>% rename(origin_name = name, lon_origin = lon, lat_origin = lat),
    by = c("origin" = "country_code")
  ) %>%
  left_join(
    centroids %>% rename(destination_name = name, lon_dest = lon, lat_dest = lat),
    by = c("destination" = "country_code")
  ) %>%
  filter(
    !is.na(lon_origin),
    !is.na(lat_origin),
    !is.na(lon_dest),
    !is.na(lat_dest)
  )

# Keep only strongest flows for readability
top_n_flows <- 100

flows_plot <- flows_map %>%
  arrange(desc(weight)) %>%
  slice_head(n = top_n_flows)

# ============================================================
# 3. World map with collaboration arrows
# ============================================================

p_flows <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey95",
    color = "grey75",
    linewidth = 0.2
  ) +
  geom_curve(
    data = flows_plot,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight
    ),
    curvature = 0.25,
    arrow = arrow(
      length = unit(0.12, "inches"),
      type = "closed"
    )
  ) +
  scale_linewidth_continuous(
    range = c(0.2, 2.5),
    name = "Collaborations"
  ) +
  scale_alpha_continuous(
    range = c(0.25, 0.8),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 80),
    expand = FALSE
  ) +
  labs(
    title = "International scientific collaboration flows involving Africa, 2025",
    subtitle = paste0("Top ", top_n_flows, " country-pair flows"),
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

p_flows

ggsave(
  "africa_scientific_collaboration_flows_2025.jpeg",
  p_flows,
  width = 14,
  height = 8,
  dpi = 500
)


library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

top_origins <- country_flows_2025 %>%
  group_by(origin) %>%
  summarise(total = sum(weight), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 6) %>%
  pull(origin)

flows_plot <- flows_map %>%
  filter(origin %in% top_origins) %>%
  group_by(origin) %>%
  slice_max(weight, n = 10, with_ties = FALSE) %>%
  ungroup()

p <- ggplot() +
  geom_sf(data = world, fill = "grey96", color = "grey80", linewidth = 0.15) +
  geom_curve(
    data = flows_plot,
    aes(
      x = lon_origin, y = lat_origin,
      xend = lon_dest, yend = lat_dest,
      linewidth = weight
    ),
    curvature = 0.25,
    alpha = 0.45,
    arrow = arrow(length = unit(0.08, "inches"), type = "closed")
  ) +
  facet_wrap(~ origin, ncol = 3) +
  scale_linewidth(range = c(0.2, 1.8), name = "Collaborations") +
  coord_sf(xlim = c(-120, 160), ylim = c(-45, 70), expand = FALSE) +
  labs(
    title = "Main international collaboration flows by African country, 2025",
    subtitle = "Top 10 partner-country flows for the six largest African origins",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

p
ggsave("collaboration_flows_faceted.jpeg", p, width = 14, height = 9, dpi = 500)






library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ------------------------------------------------------------
# 1. Keep only intra-African flows
# ------------------------------------------------------------

intra_africa_flows <- country_flows_2025 %>%
  filter(
    origin %in% african_codes,
    destination %in% african_codes,
    origin != destination,
    weight > 0
  )

# Optional: keep only strongest links for readability
top_n_flows <- 80

intra_africa_flows_top <- intra_africa_flows %>%
  arrange(desc(weight)) %>%
  slice_head(n = top_n_flows)

# ------------------------------------------------------------
# 2. Africa map + centroids
# ------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

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
  select(country_code, name, lon, lat)

# ------------------------------------------------------------
# 3. Add coordinates
# ------------------------------------------------------------

intra_africa_map <- intra_africa_flows_top %>%
  left_join(
    centroids %>%
      rename(origin_name = name, lon_origin = lon, lat_origin = lat),
    by = c("origin" = "country_code")
  ) %>%
  left_join(
    centroids %>%
      rename(destination_name = name, lon_dest = lon, lat_dest = lat),
    by = c("destination" = "country_code")
  ) %>%
  filter(
    !is.na(lon_origin),
    !is.na(lat_origin),
    !is.na(lon_dest),
    !is.na(lat_dest)
  )

# ------------------------------------------------------------
# 4. Plot intra-African collaboration map
# ------------------------------------------------------------

p_intra_africa <- ggplot() +
  geom_sf(
    data = africa_map,
    fill = "grey95",
    color = "grey75",
    linewidth = 0.25
  ) +
  geom_curve(
    data = intra_africa_map,
    aes(
      x = lon_origin,
      y = lat_origin,
      xend = lon_dest,
      yend = lat_dest,
      linewidth = weight,
      alpha = weight
    ),
    curvature = 0.25,
    arrow = arrow(
      length = unit(0.10, "inches"),
      type = "closed"
    )
  ) +
  scale_linewidth_continuous(
    range = c(0.2, 2.5),
    name = "Collaborations"
  ) +
  scale_alpha_continuous(
    range = c(0.25, 0.8),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-20, 55),
    ylim = c(-37, 38),
    expand = FALSE
  ) +
  labs(
    title = "Intra-African scientific collaboration flows, 2025",
    subtitle = paste0("Top ", top_n_flows, " country-pair flows"),
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

p_intra_africa

ggsave(
  "intra_african_scientific_collaboration_flows_2025.jpeg",
  p_intra_africa,
  width = 9,
  height = 9,
  dpi = 500
)





library(RColorBrewer)

geom_curve(
  data = intra_africa_map,
  aes(
    x = lon_origin,
    y = lat_origin,
    xend = lon_dest,
    yend = lat_dest,
    linewidth = weight,
    colour = weight
  ),
  curvature = 0.25,
  alpha = 0.85,
  arrow = arrow(
    length = unit(0.10, "inches"),
    type = "closed"
  )
) +

scale_colour_distiller(
  palette = "Reds",
  direction = 1,
  name = "Collaborations"
) +

scale_linewidth_continuous(
  range = c(0.3, 2.8),
  guide = "none"
)







library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# country_flows_2025 must have:
# origin | destination | weight

# ------------------------------------------------------------
# 1. Define African codes if needed
# ------------------------------------------------------------

african_codes <- c(
  "DZ", "AO", "BJ", "BW", "BF", "BI", "CV", "CM", "CF", "TD",
  "KM", "CG", "CD", "CI", "DJ", "EG", "GQ", "ER", "SZ", "ET",
  "GA", "GM", "GH", "GN", "GW", "KE", "LS", "LR", "LY", "MG",
  "MW", "ML", "MR", "MU", "MA", "MZ", "NA", "NE", "NG", "RW",
  "ST", "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "TZ", "TG",
  "TN", "UG", "ZM", "ZW"
)

# ------------------------------------------------------------
# 2. Keep all intra-African flows
# ------------------------------------------------------------

intra_africa_all <- country_flows_2025 %>%
  filter(
    origin %in% african_codes,
    destination %in% african_codes,
    origin != destination,
    weight > 0
  )

# ------------------------------------------------------------
# 3. Identify top origin countries
# ------------------------------------------------------------

n_top_origins <- 5

top_origin_codes <- intra_africa_all %>%
  group_by(origin) %>%
  summarise(total_collab = sum(weight), .groups = "drop") %>%
  arrange(desc(total_collab)) %>%
  slice_head(n = n_top_origins) %>%
  pull(origin)

# ------------------------------------------------------------
# 4. Classify flows: top-origin vs rest
# ------------------------------------------------------------

intra_africa_all <- intra_africa_all %>%
  mutate(
    origin_group = if_else(
      origin %in% top_origin_codes,
      "Top origin countries",
      "Other African countries"
    )
  )

# Optional: remove extremely tiny flows for readability
# Set min_weight <- 1 to keep everything
min_weight <- 1

intra_africa_all <- intra_africa_all %>%
  filter(weight >= min_weight)

# ------------------------------------------------------------
# 5. Africa map and centroids
# ------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

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

# ------------------------------------------------------------
# 6. Add coordinates
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 7. Separate top and rest for two colour gradients
# ------------------------------------------------------------

flows_rest <- intra_africa_map %>%
  filter(origin_group == "Other African countries")

flows_top <- intra_africa_map %>%
  filter(origin_group == "Top origin countries")

# Rescale weights separately so each group has its own gradient intensity
flows_rest <- flows_rest %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.2, 1)))

flows_top <- flows_top %>%
  mutate(weight_scaled = scales::rescale(weight, to = c(0.2, 1)))

# ------------------------------------------------------------
# 8. Plot
# ------------------------------------------------------------

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
  "intra_african_collaboration_flows_all_top_vs_rest_2025.jpeg",
  p_intra_africa_all,
  width = 16,
  height = 9,
  dpi = 500
)







library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# country_flows_2025 must have:
# origin | destination | weight
# origin = African country
# destination = partner country

# ------------------------------------------------------------
# 1. Define African country codes if needed
# ------------------------------------------------------------

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
