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